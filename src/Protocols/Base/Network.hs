module Protocols.Base.Network where

import           Control.Parallel.Strategies
import           Functions.Transfer
import           Protocols.Base.Router
import           Utilities.Parallel          (chunkSize)

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

showConds :: [TfCondition] -> String
showConds fpConds = unlines (map show fpConds)
