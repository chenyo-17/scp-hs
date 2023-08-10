module Protocols.Base.Network where

import           Control.Parallel.Strategies
import           Data.Maybe
import           Functions.Transfer
import           GHC.Conc                    (numCapabilities)
import           Protocols.Base.Router

-- FIXME: protocol information is lost!
newtype NetProtoTf =
  NetProtoTf Tf
  deriving (Eq)

instance Show NetProtoTf where
  show (NetProtoTf tf) = "netTf:\n" ++ show tf

-- given a list of router proto tfs of the same protocol,
-- convert them to a net proto tf
-- TODO: specify node ordering
toNetProtoTf :: [RouterProtoTf] -> NetProtoTf
toNetProtoTf rTfs =
  NetProtoTf
    $ Tf (mapMaybe mergeRTfs prodTfClauses `using` parListChunk chunkSize rpar)
  where
    chunkSize = length prodTfClauses `div` numCapabilities
    prodTfClauses :: [[TfClause]]
    prodTfClauses = mapM (tfClauses . rTf) rTfs
    -- given a list of tf clauses, merge them to a net tf clause
    mergeRTfs :: [TfClause] -> Maybe TfClause
    -- not add false conditions
    mergeRTfs tfCs =
      case tfCond newClause of
        TfFalse -> Nothing
        _       -> Just newClause
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

-- given a net proto tf, compute the fixed point for each tf clause
-- the fix point is computed by substituting each cond var with the corresponding assign var
-- and then convert assign to cond and append to the existing cond
toFpCond :: NetProtoTf -> [TfCondition]
toFpCond (NetProtoTf (Tf nTfCs)) =
  mapMaybe toEachCond nTfCs `using` parList rpar
  where
    toEachCond :: TfClause -> Maybe TfCondition
    toEachCond (TfClause nCond nAss) =
      case newCond of
        TfFalse -> Nothing
        _       -> Just newCond
      where
        newCond =
          simplifyCond $ TfAnd (substCond nCond nAss) (assignToCond nAss)

showFpCond :: [TfCondition] -> String
showFpCond fpConds = "FpCond: \n" ++ unlines (map show fpConds)
