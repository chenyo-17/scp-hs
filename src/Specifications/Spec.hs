{-# LANGUAGE GADTs #-}

module Specifications.Spec where

import           Control.Parallel.Strategies
import           Data.Maybe                  (mapMaybe)
import           Functions.Transfer
import           GHC.Conc
import           Protocols.Base.Network
import           Protocols.Base.Protocol
import Functions.Solver

-- different types of spec
-- TODO: add reachability
data SpecItem where
  RouterState :: ProtoAttr a => AttrSpec a -> SpecItem

instance Show SpecItem where
  show (RouterState attrSpec) = show attrSpec

--   Reachability :: RouterId -> RouterId -> SpecItem
-- hide a type in a list
-- data SpecItem = forall a. ProtoAttr a => MkSpecItem (SpecItem_ a)
type Spec = [SpecItem]

-- the left/right of an attrCond can be either a router id, or a specific value
data AttrCondExpr
  = Router RouterId
  | Const String
  deriving (Eq)

instance Show AttrCondExpr where
  show (Router rId) = show rId
  show (Const s)    = s

-- TODO: pre define normal spec, e.g., reachability
data AttrSpec a = AttrSpec
  { attrType  :: a
  , specLeft  :: AttrCondExpr
  , specOp    :: TfOp
  , specRight :: AttrCondExpr
  } deriving (Eq)

toAttrSpec :: a -> AttrCondExpr -> TfOp -> AttrCondExpr -> AttrSpec a
toAttrSpec = AttrSpec

instance (Show a, ProtoAttr a) => Show (AttrSpec a) where
  show spec = show $ attrSpecToCond spec

-- convert an attrSpec to a tf condition
attrSpecToCond :: ProtoAttr a => AttrSpec a -> TfCondition
attrSpecToCond (AttrSpec attr left op right) = TfCond leftExpr op rightExpr
  where
    leftExpr = toExpr left
    rightExpr = toExpr right
    toExpr :: AttrCondExpr -> TfExpr
    toExpr condExpr =
      case condExpr of
            -- TODO: how to make sure it is consistent with appendAssignVar
        Router rId -> appendExprVar (show rId) (attrToTfExpr attr)
        Const s    -> strToAttrValExpr attr s

-- convert a list of spec items to a tf condition
specToCond :: Spec -> TfCondition
specToCond spec = simplifyCond $ foldr concatSpec TfTrue spec
  where
    concatSpec :: SpecItem -> TfCondition -> TfCondition
    concatSpec _ TfFalse = TfFalse
    concatSpec (RouterState attrSpec) cond =
      cond `TfAnd` attrSpecToCond attrSpec

-- all items should be satisfied
-- TfAnd each fixed point condition with the and of all spec items
-- and return a list of tf conditions which are not false
toSpecCond :: FixedPoints -> Spec -> [TfCondition]
toSpecCond fps spec = mapMaybe concatFp fps `using` parListChunk chunkSize rpar
  where
    chunkSize = length fps `div` numCapabilities
    specCond = specToCond spec
    concatFp :: TfCondition -> Maybe TfCondition
    concatFp fp =
      case newCond of
        TfFalse -> Nothing
        _       -> Just newCond
      where
        (newCond, model) = simplifyCondWithSolver $ fp `TfAnd` specCond