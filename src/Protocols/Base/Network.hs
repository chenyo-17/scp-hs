module Protocols.Base.Network where

import           Functions.Transfer
import           Protocols.Base.Router

-- FIXME: protocol information is lost!
-- TODO: make sure it supports lazy evaluation
newtype NetProtoTf =
  NetProtoTf Tf
  deriving (Eq)

instance Show NetProtoTf where
  show (NetProtoTf tf) = "netTf:\n" ++ show tf

-- given a list of router proto tfs of the same protocol,
-- convert them to a net proto tf
-- TODO: specify node ordering

toNetProtoTf :: [RouterProtoTf] -> NetProtoTf
toNetProtoTf rTfs = foldr mergeRTfs (NetProtoTf (Tf [])) prodTfClauses
  where
    prodTfClauses :: [[TfClause]]
    prodTfClauses = mapM (tfClauses . rTf) rTfs
        -- given a list of tf clauses, merge them to a net tf clause
    mergeRTfs :: [TfClause] -> NetProtoTf -> NetProtoTf
    -- not add false conditions
    mergeRTfs tfCs accTf@(NetProtoTf (Tf accTfCs)) =
      case newCond of
        TfFalse -> accTf
        _       -> NetProtoTf (Tf (newClause : accTfCs))
      where
        newClause = TfClause newCond newAssign
        newCond = simplifyCond $ foldr (TfAnd . tfCond) TfTrue tfCs
        newAssign = foldr (concat2Assigns . tfAssign) TfAssignNull tfCs
