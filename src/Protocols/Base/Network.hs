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
      case tfCond newClause of
        TfFalse -> accTf
        _       -> NetProtoTf (Tf (newClause : accTfCs))
      where
        newClause = foldr concatClauses (TfClause TfTrue TfAssignNull) tfCs
        -- for each new clause, first substitute last assign
        -- then simplify
        concatClauses :: TfClause -> TfClause -> TfClause
        concatClauses acc@(TfClause TfFalse _) _ = acc
        concatClauses (TfClause cond1 ass1) (TfClause cond2 ass2) =
          TfClause
            (simplifyCond $ TfAnd cond1 cond2')
            (concat2Assigns ass1 ass2)
          where
            cond2' = substCond cond2 ass1
