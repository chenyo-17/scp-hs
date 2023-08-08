module Protocols.Base.Router where

import           Data.Maybe              (catMaybes)
import           Functions.Transfer
import           Protocols.Base.Protocol

-- from router tf, the tf is no longer proto tf
data RouterProtoTf = RouterProtoTf
  { router :: RouterId
  , rTf    :: Tf
  } deriving (Eq)

instance Show RouterProtoTf where
  show (RouterProtoTf rId tf) = "routerTf " ++ show rId ++ ":\n" ++ show tf

-- given a list of link tfs which has the same source router,
-- convert them to a router tf
toRouterProtoTf :: Route a => [LinkProtoTf a] -> RouterProtoTf
-- the list cannot be empty, if it is, return a dummy router tf
toRouterProtoTf [] = RouterProtoTf 0 (Tf [])
toRouterProtoTf lTfs@(LinkProtoTf (Link src _) _:_) = RouterProtoTf src routerTf
  where
    routerTf = computeRouterTf (Tf []) lTfs
    -- given the current Tf, and a list of link tfs,
    -- combing the head linkTf and each of the rest linkTfs and compute the new pTf clauses
    computeRouterTf :: Route a => Tf -> [LinkProtoTf a] -> Tf
    computeRouterTf accPTf [] = accPTf
    computeRouterTf accPTf (LinkProtoTf (Link _ cDst) curPTf:restLTfs) =
      computeRouterTf accPTf' restLTfs
        -- pass curPTf to make it clear that curPTf and restPTf are the same type
      where
        accPTf' = foldr (concatPTfs curPTf) accPTf restLTfs
        concatPTfs :: Route a => ProtoTf a -> LinkProtoTf a -> Tf -> Tf
        -- combine curTf and each of restTfs, and update the accTf
        concatPTfs curPTf' (LinkProtoTf (Link _ rDst) restPTf) accPTf2 =
          Tf (tfClauses accPTf2 ++ newTfClauses)
          where
            newTfClauses =
              foldr
                (mergeLTfClauses (cDst, rDst))
                []
                (prod2LPTfs curPTf' restPTf)
            -- given a pair of link protoTfs, product each protoTfClause pair
            prod2LPTfs ::
                 Route a
              => ProtoTf a
              -> ProtoTf a
              -> [(ProtoTfClause a, ProtoTfClause a)]
            prod2LPTfs pTf1 pTf2 =
              [(c1, c2) | c1 <- pTfClauses pTf1, c2 <- pTfClauses pTf2]

-- given a pair of pTf clauses ca and cb, and their associated dst routers,
-- compute the conditions when ra is better than rb
-- if the condition is not false, and the condition and the conditions of ca and cb to c'
-- and construct the new clause
-- for router tf, if the assign is null, can discard
-- from router tf, the tf becomes normal tf, not proto tf
mergeLTfClauses ::
     (Route a)
  => (RouterId, RouterId)
  -> (ProtoTfClause a, ProtoTfClause a)
  -> [TfClause]
  -> [TfClause]
mergeLTfClauses (r1, r2) (ProtoTfClause cond1 rte1, ProtoTfClause cond2 rte2) accC =
  accC'
  where
    -- compute the conditions when rte1 is better than rte2, and vice versa
    ass1 :: TfAssign
    ass2 :: TfAssign
    ass1 = appendAssignVal (show r1) $ maybe TfAssignNull toTfAssign rte1
    ass2 = appendAssignVal (show r2) $ maybe TfAssignNull toTfAssign rte2
    newCond =
      TfAnd (appendCondVar (show r1) cond1) (appendCondVar (show r2) cond2)
    -- concat all conditions
    accC' = accC ++ catMaybes [newC1, newC2]
    newC1 = preferFstClause rte1 ass1 ass2
    newC2 = preferFstClause rte2 ass2 ass1
    preferFstClause ::
         Route a => Maybe a -> TfAssign -> TfAssign -> Maybe TfClause
    preferFstClause rte ass1' ass2' =
      case newCond' of
        TfFalse -> Nothing
        _       -> Just $ TfClause newCond' ass1'
        -- and all 3 conditions
      where
        newCond' = simplifyCond $ TfAnd newCond (preferFstCond rte ass1' ass2')
