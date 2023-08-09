module Protocols.Base.Router where

import           Data.Maybe              (catMaybes, isNothing, mapMaybe)
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
toRouterProtoTf lTfs@(LinkProtoTf (Link src _) _:_) =
  RouterProtoTf src (Tf $ routerTf ++ toRouterNullTf src lTfs)
  where
    routerTf = computeRouterTf [] lTfs
    -- given the current Tf, and a list of link tfs,
    -- combing the head linkTf and each of the rest linkTfs and compute the new pTf clauses
    computeRouterTf :: Route a => [TfClause] -> [LinkProtoTf a] -> [TfClause]
    computeRouterTf accTf [] = accTf
    computeRouterTf accTf (LinkProtoTf (Link _ cDst) curPTf:restLTfs) =
      computeRouterTf accTf' restLTfs
        -- pass curPTf to make it clear that curPTf and restPTf are the same type
      where
        accTf' = foldr (concatPTfs curPTf) accTf restLTfs
        concatPTfs ::
             Route a => ProtoTf a -> LinkProtoTf a -> [TfClause] -> [TfClause]
        -- combine curTf and each of restTfs, and update the accTf
        concatPTfs curPTf_ (LinkProtoTf (Link _ rDst) restPTf) accPTf2 =
          accPTf2 ++ newTfClauses
          where
            newTfClauses =
              foldr
                (mergeLTfClauses src (cDst, rDst))
                []
                (prod2LPTfs curPTf_ restPTf)
            -- given a pair of link protoTfs, product each protoTfClause pair
            prod2LPTfs ::
                 Route a
              => ProtoTf a
              -> ProtoTf a
              -> [(ProtoTfClause a, ProtoTfClause a)]
            prod2LPTfs pTf1 pTf2 =
              [(c1, c2) | c1 <- pTfClauses pTf1, c2 <- pTfClauses pTf2]

-- given a list of link tfs and the source id,
-- filter all null tf clauses in each link tf,
-- if all link tf exists null tf clauses, concat them
-- not first use any to check whether all lTfs have null tf clauses is because
-- in practice it is rare, e.g., when consider session failure
-- FIXME: this is inefficient and ugly! Try not to go through all lin tfs again
toRouterNullTf :: Route a => RouterId -> [LinkProtoTf a] -> [TfClause]
toRouterNullTf src lTfs =
  if length (nullLTfClauses lTfs) == length lTfs
    then prodNullTfClauses (nullLTfClauses lTfs)
  -- if two lists do not have the same length,
  -- means there is a link tf that does not have null clauses
    else []
  where
    nullLTfClauses :: Route a => [LinkProtoTf a] -> [ProtoTf a]
    nullLTfClauses = mapMaybe onlyNullLTfClauses
    -- convert each link tf to a link tf where only null tf clauses remain
    -- if such link tf does not exist, return Nothing
    onlyNullLTfClauses :: Route a => LinkProtoTf a -> Maybe (ProtoTf a)
    onlyNullLTfClauses (LinkProtoTf (Link _ dst) pTf) =
      if null (nullClauses pTf)
        then Nothing
        else Just pTf'
      where
        isNullTfClause :: Route a => ProtoTfClause a -> Bool
        isNullTfClause (ProtoTfClause _ rte) = isNothing rte
        -- filter null tf clauses
        -- the first argument is to make the type clear
        nullClauses :: Route a => ProtoTf a -> [ProtoTfClause a]
        nullClauses pTf_ = filter isNullTfClause $ pTfClauses pTf_
        pTf' = ProtoTf (map (appendTfCond (show dst)) $ nullClauses pTf)
        -- append link dst to the condition variables
        appendTfCond :: String -> ProtoTfClause a -> ProtoTfClause a
        appendTfCond s (ProtoTfClause cond rte) =
          ProtoTfClause (appendCondVar s cond) rte
    -- given a list of link tfs where each link tf only has null tf clauses,
    -- product all null tf clauses
    prodNullTfClauses :: Route a => [ProtoTf a] -> [TfClause]
    prodNullTfClauses []   = []
    prodNullTfClauses pTfs = foldr toNullTfClause [] $ mapM pTfClauses pTfs
    -- and the condition in each clauses, simplify them,
    -- construct a new tf clause with tf assign null
    toNullTfClause :: Route a => [ProtoTfClause a] -> [TfClause] -> [TfClause]
    toNullTfClause [] accC = accC
    toNullTfClause pTfCs accC =
      case newCond of
        TfFalse -> accC
        -- expand the null route to all attributes
        _       -> TfClause newCond newAssign : accC
      where
        newCond = simplifyCond $ foldr (TfAnd . pCond) TfTrue pTfCs
        newAssign = appendAssignVar (show src) (toNullTfAssign rte)
        -- get a dummy route to pass to toNullTfAssign
        rte = (pRoute . head) pTfCs

-- given a pair of pTf clauses ca and cb, and their associated dst routers,
-- compute the conditions when ra is better than rb
-- if the condition is not false, combine the condition and the conditions of ca and cb to c'
-- and construct the new clause
-- if the assign is null, discard
-- starting from router tf, the tf becomes normal tf, not proto tf
mergeLTfClauses ::
     (Route a)
  => RouterId -- src router
  -> (RouterId, RouterId) -- dst router pair
  -> (ProtoTfClause a, ProtoTfClause a)
  -> [TfClause]
  -> [TfClause]
mergeLTfClauses src (r1, r2) (ProtoTfClause cond1 rte1, ProtoTfClause cond2 rte2) accC =
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
        _       -> Just $ TfClause newCond' (appendAssignVar (show src) ass1')
        -- and all 3 conditions
      where
        newCond' = simplifyCond $ TfAnd newCond (preferFstCond rte ass1' ass2')
