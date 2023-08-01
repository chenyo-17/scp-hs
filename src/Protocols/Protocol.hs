module Protocols.Protocol where

import           Data.List
import           Data.Maybe         (fromMaybe)
import           Data.Word
import           Functions.Transfer

type RouterId = Word32

data SessionDir
  = Import
  | Export
  deriving (Show, Eq)

-- reverse the session direction
reverseDir :: SessionDir -> SessionDir
reverseDir dir =
  case dir of
    Import -> Export
    Export -> Import

-- reverse the session
reverseSs :: Session -> Session
reverseSs (Session src dir dst) = Session dst (reverseDir dir) src

-- a Import b means this is an import session at a to apply on b
-- a Export b means this is an export session at a to apply on b
data Session = Session
  { ssSrc :: RouterId
  , ssDir :: SessionDir
  , ssDst :: RouterId
  } deriving (Show, Eq)

-- (a, b) means this is a link for a to receive b's routes
-- it is associated with a tf that chaining tf of session (b, export, a)
-- and tf of session (a, import, b)
data Link = Link
  { lkSrc :: RouterId
  , lkDst :: RouterId
  } deriving (Show, Eq)

class Route a where
  mergeRoute :: a -> a -> Maybe a

data Protocol =
  BGP
  deriving (Show, Eq)

data SessionProtoTf = SessionProtoTf
  { session :: Session
  , ssProto :: Protocol
  , ssTf    :: Tf
  } deriving (Show, Eq)

data LinkProtoTf = LinkProtoTf
  { link   :: Link
  , lProto :: Protocol
  , lTf    :: Tf
  } deriving (Show, Eq)

class SessionTf a where
  addSessionTf :: Session -> a -> [SessionProtoTf] -> [SessionProtoTf]

-- convert session tfs to link tfs
-- by chaining sessionProtoTf (b export a) and (a import b) to linkProtoTf (a b)
toLinkTfs :: [SessionProtoTf] -> [LinkProtoTf]
toLinkTfs ssTfs = foldr toLinkTf [] ssTfs
  where
    toLinkTf :: SessionProtoTf -> [LinkProtoTf] -> [LinkProtoTf]
    -- given a sessionTf, first check whether its other part is already processed
    -- if so, do nothing, otherwise, first find the other part, then chain them
    toLinkTf sTf linkTfs
      | isProcessed sTf = linkTfs
      | (ssDir . session) sTf == Export -- decide the order of concatenation
       = concatSsTfs sTf (pairSsTf sTf) : linkTfs
      | otherwise = concatSsTfs (pairSsTf sTf) sTf : linkTfs
    -- check whether ssTf is already processed
    isProcessed :: SessionProtoTf -> Bool
    isProcessed = undefined
    -- get the pair of ssTf in the ssTfs
    pairSsTf :: SessionProtoTf -> SessionProtoTf
    pairSsTf sTf = fromMaybe defaultSessionTf $ find isPairSsTf ssTfs
      where
        SessionProtoTf ssa proto _ = sTf
        ssb = reverseSs ssa
        -- compute the pair session
        -- prepare the default sessionTf
        defaultSessionTf = SessionProtoTf ssb proto (Tf [])
        -- check whether the given ssTf is the pair of sTf
        isPairSsTf :: SessionProtoTf -> Bool
        isPairSsTf (SessionProtoTf ssb' proto' _) =
          ssb == ssb' && proto == proto'

-- concatenate a pairs of session tfs to one link tf
concatSsTfs :: SessionProtoTf -> SessionProtoTf -> LinkProtoTf
concatSsTfs ssTfe ssTfi = LinkProtoTf l p linkTf
  where
    ssImp = session ssTfi
    -- the link refers to the import session
    l = Link (ssSrc ssImp) (ssDst ssImp)
    p = ssProto ssTfi
    linkTf = foldr concatClauses (Tf []) (prod2Tfs ssTfe ssTfi)
        -- if 2 clauses can concat, add it to the new tf
        -- otherwise, do nothing
      where
        concatClauses :: (TfClause, TfClause) -> Tf -> Tf
        concatClauses (c1, c2) tf =
          case c' of
            Just c  -> Tf (c : tfClauses tf)
            Nothing -> tf
          where
            c' = concatTfClause (c1, c2)
        -- produce the clause product of 2 tfs
        prod2Tfs :: SessionProtoTf -> SessionProtoTf -> [(TfClause, TfClause)]
        prod2Tfs (SessionProtoTf _ _ tf1) (SessionProtoTf _ _ tf2) =
          [(c1, c2) | c1 <- tfClauses tf1, c2 <- tfClauses tf2]

-- concatenate two TfClauses to one TfClause
-- the check of the second clause cond is based on the first clause's assign
-- e.g.1, c1 = a > 10 -> a := 5
--        c2 =  a < 6 -> b := 3
--    returns c3 = a > 10 -> a := 5, b := 3
-- e.g.2, c1 = a > 10 -> b := 10
--        c2 = a < 6 -> a := 5
--    returns c3 = a > 10 -> b := 10, a := 5
-- the second assign overrides the first one if there assign the same variable
concatTfClause :: (TfClause, TfClause) -> Maybe TfClause
concatTfClause (TfClause c1 a1, TfClause c2 a2) =
  if c' == TfFalse
    then Just $ TfClause c' a'
    else Nothing
  where
    -- combine two conditions
    c' = simplifyCond $ TfAnd c1 (substCond c2 a1)
    -- combine two assigns
    a' = comb2Assigns a1 a2

-- substitute the variable in the condition with the assign
-- and simplify the condition
-- TODO: support numeric conditions, e.g., a + 10 > 20 && a < 10
substCond :: TfCondition -> TfAssign -> TfCondition
substCond = undefined

-- combine two assigns, the second one overrides the first one
comb2Assigns :: TfAssign -> TfAssign -> TfAssign
-- if some assign is null, return the other one
comb2Assigns TfAssignNull a = a
comb2Assigns a TfAssignNull = a
-- the result cannot be null, as the null case has been handled
comb2Assigns a1 a2 = TfAssign $ foldr updateOrAppend [] assignList
  where
    -- concat two assign lists into one,
    -- each element is an TfAssignItem
    -- the second is appended to the first one
    assignList =
      let TfAssign l1 = a1
          TfAssign l2 = a2
      -- reverse the order because of foldr starts from the end
       in l2 ++ l1
    -- if the variable is already assigned in the list, delete it first
    -- append the new assign to the list
    updateOrAppend :: TfAssignItem -> [TfAssignItem] -> [TfAssignItem]
    updateOrAppend (TfAssignItem v e) as = as' ++ [TfAssignItem v e]
      where
        as' = filter (\(TfAssignItem v' _) -> v /= v') as
