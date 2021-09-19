{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
-- | This module is extracted from @ghc-prof@.
module GHC.Prof.BuildTree (
    buildTree,
    Level,
) where

import GHC.Prof.Types

import Data.Maybe (listToMaybe)
import Data.Foldable (foldl')
import Data.Text (Text)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

seqM :: Monad m => m a -> m a
seqM m = do
  a <- m
  return $! a

type Level = Int

-- | TreePath represents a path to a node in a cost centre tree.
--
-- Invariant: @'treePathLevel' == length 'treePath'@
data TreePath = TreePath
  { treePathLevel :: !Level
  -- ^ Current depth of the path
  , treePath :: [CostCentreNo]
  -- ^ Path to the node
  }

push :: CostCentreNo -> TreePath -> TreePath
push ccNo path@TreePath {..} = path
  { treePathLevel = treePathLevel + 1
  , treePath = ccNo:treePath
  }

popTo :: Level -> TreePath -> TreePath
popTo level path@TreePath {..} = path
  { treePathLevel = level
  , treePath = drop (treePathLevel - level) treePath
  }

currentNo :: TreePath -> Maybe CostCentreNo
currentNo TreePath {treePath} = listToMaybe treePath

buildTree :: [(Level, CostCentre)] -> CostCentreTree
buildTree = snd . foldl' go (TreePath 0 [], emptyCostCentreTree)
  where
    go
      :: (TreePath, CostCentreTree)
      -> (Level, CostCentre)
      -> (TreePath, CostCentreTree)
    go (!path, !CostCentreTree {..}) (level, node) = (path', tree')
      where
        ccNo = costCentreNo node
        parentPath = popTo level path
        parentNo = currentNo parentPath
        path' = push ccNo parentPath
        tree' = CostCentreTree
          { costCentreNodes = IntMap.insert ccNo node costCentreNodes
          , costCentreParents = maybe costCentreParents
            (\parent -> IntMap.insert ccNo parent costCentreParents)
            parentNo
          , costCentreChildren = maybe costCentreChildren
            (\parent -> IntMap.insertWith Set.union parent
              (Set.singleton node)
              costCentreChildren)
            parentNo
          , costCentreCallSites = Map.insertWith Set.union
            (costCentreName node, costCentreModule node)
            (Set.singleton node)
            costCentreCallSites
          , costCentreAggregate = Map.alter
            (Just . updateCostCentre)
            (costCentreModule node)
            costCentreAggregate
          }
        aggregate = AggregatedCostCentre
          { aggregatedCostCentreName = costCentreName node
          , aggregatedCostCentreModule = costCentreModule node
          , aggregatedCostCentreSrc = costCentreSrc node
          , aggregatedCostCentreEntries = Just $! costCentreEntries node
          , aggregatedCostCentreTime = costCentreIndTime node
          , aggregatedCostCentreAlloc = costCentreIndAlloc node
          , aggregatedCostCentreTicks = costCentreTicks node
          , aggregatedCostCentreBytes = costCentreBytes node
          }
        updateCostCentre
          :: Maybe (Map.Map Text AggregatedCostCentre)
          -> Map.Map Text AggregatedCostCentre
        updateCostCentre = \case
          Nothing -> Map.singleton (costCentreName node) aggregate
          Just costCentreByName ->
            Map.insertWith
              addCostCentre
              (costCentreName node)
              aggregate
              costCentreByName
        addCostCentre x y = x
          { aggregatedCostCentreEntries = seqM $ (+)
            <$> aggregatedCostCentreEntries x
            <*> aggregatedCostCentreEntries y
          , aggregatedCostCentreTime =
            aggregatedCostCentreTime x + aggregatedCostCentreTime y
          , aggregatedCostCentreAlloc =
            aggregatedCostCentreAlloc x + aggregatedCostCentreAlloc y
          , aggregatedCostCentreTicks = seqM $ (+)
            <$> aggregatedCostCentreTicks x
            <*> aggregatedCostCentreTicks y
          , aggregatedCostCentreBytes = seqM $ (+)
            <$> aggregatedCostCentreBytes x
            <*> aggregatedCostCentreBytes y
          }
