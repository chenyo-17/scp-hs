module Protocols.Base.Network where

import           Control.Parallel.Strategies                    
import           Data.Maybe                                     (catMaybes,
                                                                 mapMaybe)
import           Functions.Transfer
import           Protocols.Base.Router
import Utilities.Parallel
import qualified Streamly.Prelude as S
import Streamly

newtype NetProtoTf = NetProtoTf
  { nTf :: Tf
  } deriving (Eq)

instance Show NetProtoTf where
  show (NetProtoTf tf) = "netTf:\n" ++ show tf

type FixedPoints = [TfCondition]
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

toNetProtoTf :: [RouterProtoTf] -> NetProtoTf
toNetProtoTf rTfs =
  NetProtoTf $ Tf $ concat $ parMap rseq processChunk (chunks 1000 prodTfClauses)
  where
    -- prodTfClauses :: [[TfClause]]
    -- prodTfClauses = mapM (tfClauses . rTf) rTfs
    prodTfClauses :: [[TfClause]]
    prodTfClauses = sequence (map (tfClauses .rTf) rTfs)
    
    processChunk :: [[TfClause]] -> [TfClause]
    processChunk chunk = catMaybes $ map mergeRTfs chunk
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
        TfClause cond assign =
          foldr concatClauses (TfClause TfTrue (TfAssign [])) tfCs
        -- unwrap assign
        newClause = TfClause cond (unwrapAssign assign)
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
