{-# LANGUAGE BangPatterns      #-}
module Main (main) where

import Data.Foldable      (traverse_)
import Data.List          (intercalate)
import Data.Tree          (Tree (..))
import GHC.Events2Prof    (eventlogToProfile)
import System.Environment (getArgs)
import Text.Printf        (printf)

import qualified Data.Scientific as Sci
import qualified Data.Text       as T
import qualified Data.Text.IO    as T.IO
import qualified GHC.Prof        as Prof
import qualified GHC.RTS.Events  as Ev

main :: IO ()
main = do
    args <- getArgs
    traverse_ process args

process :: FilePath -> IO ()
process fp = do
    mev <- Ev.readEventLogFromFile fp
    either putStrLn process1 mev

process1 :: Ev.EventLog -> IO ()
process1 ev = do
    T.IO.putStrLn (Prof.profileCommandLine prof)
    putStrLn ""
    printf "total time = %.02f secs (%d ticks @ %d us)\n"
        (realToFrac $ Prof.totalTimeElapsed totalTime :: Double)
        (Prof.totalTimeTicks totalTime)
        (round $ Prof.totalTimeResolution totalTime :: Int)
    -- total alloc
    putStrLn ""
    putStr $ table $ header $ maybe [] (go 0) $ Prof.costCentres prof
  where
    prof      = eventlogToProfile ev
    totalTime = Prof.profileTotalTime prof

    header = (:)
        [ "COST CENTRE"
        , "MODULE"
        , "SRC"
        , "entries"
        , "%time"
        , "%alloc"
        , "%time"
        , "%alloc"
        , "ticks"
        , "bytes"
        ]

    go :: Int -> Tree Prof.CostCentre -> [[String]]
    go lvl (Node cc ccs) =
      [ replicate lvl ' ' ++ T.unpack (Prof.costCentreName cc)
      , T.unpack (Prof.costCentreModule cc)
      , maybe "?" T.unpack (Prof.costCentreSrc cc)
      , printf "%d" (Prof.costCentreEntries cc)
      , printf "%.01f" (Sci.toRealFloat $ Prof.costCentreIndTime cc :: Double)
      , printf "%.01f" (Sci.toRealFloat $ Prof.costCentreIndAlloc cc :: Double)
      , printf "%.01f" (Sci.toRealFloat $ Prof.costCentreInhTime cc :: Double)
      , printf "%.01f" (Sci.toRealFloat $ Prof.costCentreInhAlloc cc :: Double)
      , maybe "?" (printf "%d") (Prof.costCentreTicks cc)
      , maybe "?" (printf "%d") (Prof.costCentreBytes cc)
      ]
      : concatMap (go (lvl + 1)) ccs

table :: [[String]] -> String
table cells = unlines rows
  where
    cols      :: Int
    rowWidths :: [Int]
    rows      :: [String]

    (cols, rowWidths, rows) = foldr go (0, repeat 0, []) cells

    go :: [String] -> (Int, [Int], [String]) -> (Int, [Int], [String])
    go xs (c, w, yss) =
        ( max c (length xs)
        , zipWith max w (map length xs ++ repeat 0)
        , unwords2 (take cols (zipWith fill xs rowWidths))
          : yss
        )

    fill :: String -> Int -> String
    fill s n = s ++ replicate (n - length s) ' '

    unwords2 = intercalate "  "
