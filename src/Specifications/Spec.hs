{-# LANGUAGE GADTs #-}

module Specifications.Spec where

import           Functions.Transfer
import           Protocols.Base.Network
import           Protocols.Base.Protocol

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
  , specOp    :: TfOpA
  , specRight :: AttrCondExpr
  } deriving (Eq)

toAttrSpec :: a -> AttrCondExpr -> TfOpA -> AttrCondExpr -> AttrSpec a
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

-- given a net proto tf and a spec, return a list of tf conditions under which
-- the spec is satisfied, the condition only contains environmental variables,
-- e.g., variables that are not assigned in the tf
-- this function first add the spec condition to each tf clause condition,
-- then concat the condition and the assign and simplify it
-- finally, Or all the conditions
toSpecCond :: NetProtoTf -> Spec -> TfCondition
toSpecCond (NetProtoTf (Tf nTfCs)) spec =
  simplifyCond (foldr TfOr TfFalse $ filter noFalse $ map stepClause nTfCs)
  where
    specCond = specToCond spec
    stepClause :: TfClause -> TfCondition
    stepClause tfC@(TfClause _ assign) =
      substCond (TfAnd (clauseToTfCond tfC) specCond) assign
    noFalse :: TfCondition -> Bool
    noFalse TfFalse = False
    noFalse _       = True
