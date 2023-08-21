module Protocols.Base.Network where

import           Control.Parallel.Strategies
import           Data.Maybe                  (mapMaybe)
import           Functions.Transfer
import           Protocols.Base.Router
import           Utilities.Parallel          (chunkSize)

newtype NetProtoTf =
  NetProtoTf Tf
  deriving (Eq)

instance Show NetProtoTf where
  show (NetProtoTf tf) = "netTf:\n" ++ show tf

-- given a list of router proto tfs,
-- product each tf clause and merge conditions and assigns separately
-- then substitute conditions with assigns with best effort,
-- and convert assign to cond, e.g., a := b -> a == b
-- this is required because not all assigns are used in conditions
-- FIXME: this does not guarantee complete substitution,
-- e.g., if assign = [b := 10, a := b], then a < 5, will only be converted to b < 5
-- as b := 10 is already visited
toNetFpCond :: [RouterProtoTf] -> [TfCondition]
-- filter out false conditions
toNetFpCond rTfs =
  filter noFalse (map mergeRTfs prodTfClauses)
    `using` parListChunk chunkSize rpar
  where
    prodTfClauses :: [[TfClause]]
    prodTfClauses = mapM (tfClauses . rTf) rTfs
    -- given a list of tf clauses, merge them to a tf condition
    mergeRTfs :: [TfClause] -> TfCondition
    mergeRTfs tfCs =
      clauseToTfCond (foldr concatTfClauses (TfClause TfTrue TfAssignNull) tfCs)
    noFalse :: TfCondition -> Bool
    noFalse TfFalse = False
    noFalse _       = True

type FixedPoints = [TfCondition]

toNetProtoTf :: [RouterProtoTf] -> NetProtoTf
toNetProtoTf rTfs = NetProtoTf $ Tf (mapMaybe mergeRTfs prodTfClauses)
  where
    prodTfClauses :: [[TfClause]]
    prodTfClauses = mapM (tfClauses . rTf) rTfs
    -- given a list of tf clauses, merge them to a net tf clause
    mergeRTfs :: [TfClause] -> Maybe TfClause
    -- not add false conditions
    mergeRTfs tfCs =
      case tfCond newClause of
        TfFalse -> Nothing
        -- check whether the new clause is valid
        -- even if here we do a complete substitution, the partial substitution in
        -- concatClauses is still required, otherwise the condition cannot be fully simplified
        _ ->
          if clauseToTfCond newClause == TfFalse
            then Nothing
            else Just newClause
      where
        newClause = foldr concatClauses (TfClause TfTrue (TfAssign [])) tfCs
        -- for each new clause, first substitute last assign
        -- then simplify
        concatClauses :: TfClause -> TfClause -> TfClause
        concatClauses _ acc@(TfClause TfFalse _) = acc
        concatClauses (TfClause cond1 ass1) (TfClause cond2 ass2) =
          TfClause
            (simplifyCond $ TfAnd cond1' cond2)
            (concat2Assigns ass1 ass2)
          where
            cond1' = substCond cond1 ass2

showConds :: [TfCondition] -> String
showConds fpConds = unlines (map show fpConds)
