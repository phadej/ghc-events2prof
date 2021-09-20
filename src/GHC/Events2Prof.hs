{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.Events2Prof (
    eventlogToProfile,
    eventsToProfile,
    -- * Low-level
    Acc (..), emptyAcc, processEvent, accToProfile,
    Sample (..),
    costCentres,
) where

import Data.List      (sortBy)
import Data.Ord       (comparing)
import Data.Semigroup (Semigroup (..))
import Data.Word      (Word32, Word64)

import qualified Data.List           as L
import qualified Data.Map.Strict     as M
import qualified Data.Scientific     as S
import qualified Data.Text           as T
import qualified Data.Time           as Time
import qualified Data.Vector.Unboxed as VU
import qualified GHC.Prof            as Prof
import qualified GHC.Prof.BuildTree  as Prof
import qualified GHC.RTS.Events      as Ev

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | Convert 'Ev.Eventlog' to @ghc-prof@ 'Prof.Profile'.
eventlogToProfile :: Ev.EventLog -> Prof.Profile
eventlogToProfile ev = eventsToProfile (Ev.events (Ev.dat ev))

-- | Convert a list of 'Ev.Event's to @ghc-prof@ 'Prof.Profile'.
eventsToProfile :: [Ev.Event] -> Prof.Profile
eventsToProfile = accToProfile . L.foldl' processEvent emptyAcc

-- | Process single 'Ev.Event' into accumulator.
processEvent :: Acc -> Ev.Event -> Acc
processEvent !acc ev = case Ev.evSpec ev of
    Ev.ProgramArgs _ args ->
        acc { accCommandLine = Just args }
    Ev.ProfBegin ti ->
        acc { accTickInterval = Just ti }
    Ev.HeapProfCostCentre ccid label m loc _flags ->
        acc { accCostCentres = M.insert ccid (CallCenterInfo label m loc) (accCostCentres acc) } -- TODO: read _flags whether the ccid is a CAF
    Ev.ProfSampleCostCentre _capset _ticks _sd stack ->
        acc { accSamplesByStack = addSampleByStack stack s (accSamplesByStack acc)
            , accSamplesByCC    = addSampleByCC    top   s (accSamplesByCC acc)
            , accSamplesCount   = 1 + accSamplesCount acc
            }
      where
        s = Sample { sTicks = 1, sBytes = 0, sEntries = 0 }

        top | VU.null stack = 0
            | otherwise     = VU.head stack

    _ -> acc

-- | Convert an 'Acc'umulator to 'Prof.Profile.
accToProfile :: Acc -> Prof.Profile
accToProfile acc = Prof.Profile
    { Prof.profileTimestamp      = Time.LocalTime (Time.ModifiedJulianDay 0) (Time.TimeOfDay 0 0 0)
    , Prof.profileCommandLine    = maybe "" T.unwords (accCommandLine acc)
    , Prof.profileTotalTime      = Prof.TotalTime
        { Prof.totalTimeElapsed    = 0
        , Prof.totalTimeTicks      = toInteger totalTicks
        , Prof.totalTimeResolution = timeRes
        , Prof.totalTimeProcessors = Nothing
        }
    , Prof.profileTotalAlloc     = Prof.TotalAlloc 0
    , Prof.profileTopCostCentres = take 5 $ sortBy cmp
                                 $ aggregatedCostCentres totalTicks (accSamplesByCC acc) (accCostCentres acc)
    , Prof.profileCostCentreTree = Prof.buildTree
                                 $ costCentres totalTicks (accSamplesByStack acc) (accCostCentres acc)
    }
  where
    cmp = flip (comparing Prof.aggregatedCostCentreTime)
    -- report in microseconds.
    timeRes    = maybe 1e-9 (\ns -> fromIntegral ns / 1000) (accTickInterval acc)
    totalTicks = accSamplesCount acc

addSampleByStack
    :: VU.Vector CostCentreId
    -> Sample
    -> M.Map (VU.Vector CostCentreId) Sample
    -> M.Map (VU.Vector CostCentreId) Sample
addSampleByStack stack s m = M.insertWith (<>) stack s m

addSampleByCC
    :: CostCentreId
    -> Sample
    -> M.Map CostCentreId Sample
    -> M.Map CostCentreId Sample
addSampleByCC ccid s m = M.insertWith (<>) ccid s m

aggregatedCostCentres
    :: Word64
    -> M.Map CostCentreId Sample
    -> (M.Map CostCentreId CallCenterInfo)
    -> [Prof.AggregatedCostCentre]
aggregatedCostCentres totalTicks m0 cc0 =
    [ mkAggregatedCostCentre ccid s
    | (ccid, s) <- M.toList m0
    ]
  where
    mkAggregatedCostCentre :: CostCentreId -> Sample -> Prof.AggregatedCostCentre
    mkAggregatedCostCentre ccid s = Prof.AggregatedCostCentre
        { Prof.aggregatedCostCentreName    = cciLabel cc
        , Prof.aggregatedCostCentreModule  = cciMod cc
        , Prof.aggregatedCostCentreSrc     = Just (cciSrcLoc cc)
        , Prof.aggregatedCostCentreEntries = Just (toInteger (sEntries s))
        , Prof.aggregatedCostCentreTime    = ticksToTime (sTicks s)
        , Prof.aggregatedCostCentreAlloc   = 0 -- TODO
        , Prof.aggregatedCostCentreTicks   = Just (toInteger (sTicks s))
        , Prof.aggregatedCostCentreBytes   = Just (toInteger (sBytes s))
        }
      where
        cc = M.findWithDefault defaultCallCenterInfo ccid cc0

    ticksToTime :: Word64 -> S.Scientific
    ticksToTime ticks = S.fromFloatDigits (100 * fromIntegral ticks / fromIntegral totalTicks :: Double)

-- | Convert accumulated data into list of levels and costcentres,
-- so ghc-prof can build its CostCentreTree.
costCentres
    :: Word64  -- ^ total sample count (ticks)
    -> M.Map (VU.Vector CostCentreId) Sample
    -> (M.Map CostCentreId CallCenterInfo)
    -> [(Prof.Level, Prof.CostCentre)]
costCentres totalTicks m0 cc0 =
    [ (VU.length ccid, mkCostCentre n ccid as)
    | (n, (ccid, as)) <- zip [1..] (M.toList accSampleMap)
    ]
  where
    mkCostCentre :: Int -> VU.Vector CostCentreId -> Sample -> Prof.CostCentre
    mkCostCentre no ccid as = Prof.CostCentre
        { Prof.costCentreNo       = no
        , Prof.costCentreName     = cciLabel cc
        , Prof.costCentreModule   = cciMod cc
        , Prof.costCentreSrc      = Just (cciSrcLoc cc)
        , Prof.costCentreEntries  = toInteger (sEntries s) -- no idea what this is
        , Prof.costCentreIndTime  = ticksToTime (sTicks s)
        , Prof.costCentreIndAlloc = 0 -- TODO
        , Prof.costCentreInhTime  = ticksToTime (sTicks as)
        , Prof.costCentreInhAlloc = 0 -- TODO
        , Prof.costCentreTicks    = Just (toInteger (sTicks s))
        , Prof.costCentreBytes    = Just (toInteger (sBytes s))
        }
      where
        cc | VU.null ccid = mainCallCenterInfo
           | otherwise    = M.findWithDefault defaultCallCenterInfo (VU.last ccid) cc0
        s =  M.findWithDefault mempty (VU.reverse ccid) m0

    ticksToTime :: Word64 -> S.Scientific
    ticksToTime ticks = S.fromFloatDigits (100 * fromIntegral ticks / fromIntegral totalTicks :: Double)

    -- For each call stack we take partial callstacks and accumulate data.
    accSampleMap :: M.Map (VU.Vector CostCentreId) Sample
    accSampleMap = M.foldlWithKey' f M.empty m0
      where
        f :: M.Map (VU.Vector CostCentreId) Sample
          -> VU.Vector CostCentreId
          -> Sample
          -> M.Map (VU.Vector CostCentreId) Sample
        f m ccid s = L.foldl' (\m' ccid' -> M.insertWith (<>) ccid' s m') m (partialCallstacks ccid)

-- |
partialCallstacks :: VU.Unbox a => VU.Vector a -> [VU.Vector a]
partialCallstacks v = [ VU.take n v' | n <- [ 0 .. VU.length v ] ] where v' = VU.reverse v

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Eventlog cost centre identifier.
type CostCentreId = Word32

-- | Accumulator.
data Acc = Acc
    { accCommandLine    :: !(Maybe [T.Text])   -- ^ The command-line arguments passed to the program. @PROGRAM_ARGS@
    , accTickInterval   :: !(Maybe Word64)     -- ^ tick interval in nanoseconds. @PROF_BEGIN@
    , accCostCentres    :: !(M.Map CostCentreId CallCenterInfo)
    , accSamplesByCC    :: !(M.Map CostCentreId Sample)
    , accSamplesByStack :: !(M.Map (VU.Vector CostCentreId) Sample)
    , accSamplesCount   :: !Word64
    }
  deriving Show

emptyAcc :: Acc
emptyAcc = Acc
    { accCommandLine    = Nothing
    , accTickInterval   = Nothing
    , accCostCentres    = M.empty
    , accSamplesByCC    = M.empty
    , accSamplesByStack = M.empty
    , accSamplesCount   = 0
    }

data CallCenterInfo = CallCenterInfo
    { cciLabel  :: !T.Text
    , cciMod    :: !T.Text
    , cciSrcLoc :: !T.Text
    }
  deriving Show

-- | Top level cost center
mainCallCenterInfo :: CallCenterInfo
mainCallCenterInfo = CallCenterInfo
    { cciLabel  = "MAIN"
    , cciMod    = "MAIN"
    , cciSrcLoc = "<built-in>"
    }

defaultCallCenterInfo :: CallCenterInfo
defaultCallCenterInfo = CallCenterInfo
    { cciLabel  = "unknown"
    , cciMod    = "unknown"
    , cciSrcLoc = "UNKNOWN"
    }

data Sample = Sample
    { sTicks   :: !Word64
    , sBytes   :: !Word64 -- ^ for now, these are not extracted: https://gitlab.haskell.org/ghc/ghc/-/issues/20384
    , sEntries :: !Word64
    }
  deriving Show

instance Data.Semigroup.Semigroup Sample where
    Sample x1 x2 x3 <> Sample y1 y2 y3 = Sample (x1 + y1) (x2 + y2) (x3 + y3)

instance Monoid Sample where
    mempty = Sample 0 0 0
    mappend = (<>)
