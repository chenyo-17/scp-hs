{-# LANGUAGE GADTs #-}

module Specifications.Spec where

import           Control.Parallel.Strategies
import           Data.Maybe
import           Functions.Transfer
import           Protocols.Base.Network
import           Protocols.Base.Protocol
import           Utilities.Parallel

-- different types of spec
-- TODO: add reachability
data SpecItem where
  RouterState :: ProtoAttr a => AttrSpec a -> SpecItem

instance Show SpecItem where
  show (RouterState attrSpec) = show attrSpec

--   Reachability :: RouterId -> RouterId -> SpecItem
-- hide a type in a list
-- data SpecItem = forall a. ProtoAttr a => MkSpecItem (SpecItem_ a)
data Spec
  = SItem SpecItem
  | SpecAnd Spec Spec
  | SpecOr Spec Spec
  | SpecImply Spec Spec
  | STrue -- no specification or assumption

instance Show Spec where
  show (SItem specItem) = show specItem
  show (SpecAnd spec1 spec2) = "(" ++ show spec1 ++ ") & (" ++ show spec2 ++ ")"
  show (SpecOr spec1 spec2) = "(" ++ show spec1 ++ ") | (" ++ show spec2 ++ ")"
  show (SpecImply spec1 spec2) =
    "(" ++ show spec1 ++ ") => (" ++ show spec2 ++ ")"
  show STrue = "True"

-- FIXME: assumptions should be about routes,
-- but here it is state of routers
type Assump = Spec

-- the left/right of an attrCond can be either a router id, or a specific value
data AttrCondExpr
  = Router RouterId
  | Const String -- this is protocol specific
  | Symbol String
  deriving (Eq)

instance Show AttrCondExpr where
  show (Router rId) = show rId
  show (Const s)    = s
  show (Symbol s)   = s

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
        Symbol s   -> TfVar s

-- convert a list of spec items to a tf condition
specToCond :: Spec -> TfCondition
specToCond (SItem (RouterState attrSpec)) = attrSpecToCond attrSpec
specToCond (SpecAnd spec1 spec2) = TfAnd (specToCond spec1) (specToCond spec2)
specToCond (SpecOr spec1 spec2) = TfOr (specToCond spec1) (specToCond spec2)
specToCond (SpecImply spec1 spec2) =
  TfImply (specToCond spec1) (specToCond spec2)
specToCond STrue = TfTrue

-- given a net proto tf, a spec and an assumption,
-- return a list of tf conditions under which
-- the spec is satisfied while assumption holds,
-- the condition only contains environmental variables,
-- e.g., variables that are not assigned in the tf
-- this function first add the (negated, if the last bool argument is true) spec
-- and the assumption to each tf clause condition,
-- then concat the condition and the assign, eliminate internal variables,
-- and finally simplify the condition
toSpecCond :: NetProtoTf -> Spec -> Spec -> [TfCondition]
toSpecCond (NetProtoTf (Tf nTfCs)) assump spec =
  mapMaybe stepClause nTfCs `using` parListChunk chunkSize rpar
  where
    specCond = specToCond spec
    assumpCond = specToCond assump
    stepClause :: TfClause -> Maybe TfCondition
    stepClause tfC@(TfClause _ assign) =
      case newCond of
        TfFalse -> Nothing
        _       -> Just newCond
      where
        newCond =
          simplifyCond
            $ TfAnd
                assumpCond
                (substCond (TfAnd (clauseToTfCond tfC) specCond) assign)
