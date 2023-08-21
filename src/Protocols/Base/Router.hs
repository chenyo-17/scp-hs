module Protocols.Base.Router where

import           Data.List               (foldl')
import           Data.Maybe              (mapMaybe)
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
-- both routerTf and toRouterNullTf consist of 2 levels of map
-- TODO: the router tf should be protocol independent, it should combine multiple protocols,
-- and indicate which protocol is used, e.g., when 2 protocols learn the same prefix
toRouterProtoTf :: Route a => [LinkProtoTf a] -> RouterProtoTf
-- the list cannot be empty, if it is, return a dummy router tf
toRouterProtoTf [] = RouterProtoTf 0 (Tf [])
toRouterProtoTf lTfs@(LinkProtoTf (Link src _) _:_) =
  RouterProtoTf src (Tf $ routerTf ++ toRouterNullTf lTfs)
  where
    -- only append assign var when the router f is final!
    -- otherwise cannot extract route attribute from assign
    routerTf =
      map (\x -> x {tfAssign = appendAssignVar (show src) (tfAssign x)})
        $ computeRouterTf lTfs
    -- given the current Tf, and a list of link tfs,
    -- combine the head linkTf and each of the rest linkTfs and compute the new pTf clauses
    computeRouterTf :: Route a => [LinkProtoTf a] -> [TfClause]
    computeRouterTf [] = []
    computeRouterTf lTfs_ = foldl' concatPTfs [] lTfs_
        -- the input and output tf clauses list are completely different
      where
        concatPTfs :: Route a => [TfClause] -> LinkProtoTf a -> [TfClause]
        -- for each ltf, combine it with each of the accTf clauses
        -- e.g., lTfs = [(a1,a2), (b1, b2), (c1, c2)]
        -- step 1: accTf = [c1, c2]
        -- step 2: accTf = [b1 > c1, c1 > b1, b1 > c2, c2 > b1,...]
        -- step 3: accTf = [a1 > (b1 > c1), a1 < (b1 > c1),...]
        -- for the first lTf, just unwrap all clauses
        concatPTfs [] (LinkProtoTf (Link _ dst) (ProtoTf pTfCs)) =
          map toTfClause pTfCs
              -- append dst to cond vars, and convert route to tf assign
              -- and append dst to assign val
          where
            toTfClause :: Route a => ProtoTfClause a -> TfClause
            toTfClause (ProtoTfClause pc pr) = TfClause cond ass
              where
                cond = appendCondVar (show dst) pc
                ass = appendAssignVal (show dst) (toTfAssign pr)
        -- for every pair of accTf and pTf clauses,
        -- merge them and add to the new list
        -- accTfCs is always rewritten
        concatPTfs accTfCs (LinkProtoTf (Link _ dst) (ProtoTf pTfCs)) =
          concatMap mergeTfClauses pTfCs
          -- not use chunk as pTfCs is usually smaller than accTfs
          -- and the computation for each clause is intensive
          where
            mergeTfClauses :: Route a => ProtoTfClause a -> [TfClause]
            -- given a pTf clause, compare it with each of the accTf clauses
            -- and add to accTfCs
            mergeTfClauses newPTfC = concatMap preferClause accTfCs
              where
                preferClause :: TfClause -> [TfClause]
                preferClause oldTfC =
                  mapMaybe prefer [preferOldCond, preferNewCond]
                  where
                    prefer cond =
                      if newCond == TfFalse
                        then Nothing
                        else Just $ TfClause newCond (selectAssign cond)
                      where
                        newCond = simplifyCond $ TfAnd curCond cond
                    selectAssign cond
                      | cond == preferOldCond = oldAssign
                      | otherwise = newAssign
                    newRte = pRoute newPTfC
                    oldAssign = tfAssign oldTfC
                    newAssign = appendAssignVal (show dst) (toTfAssign newRte)
                    preferOldCond = preferFstCond newRte oldAssign newAssign
                    preferNewCond = preferFstCond newRte newAssign oldAssign
                    curCond =
                      TfAnd
                        (tfCond oldTfC)
                        (appendCondVar (show dst) (pCond newPTfC))

-- given a list of link tfs and the source id (used to append assign var),
-- filter all null tf clauses in each link tf,
-- if all link tf exists null tf clauses, concat them
toRouterNullTf :: Route a => [LinkProtoTf a] -> [TfClause]
toRouterNullTf [] = []
-- use foldr because once it returns [], it will directly return
-- use a dummy initial list
toRouterNullTf lPTfs = foldr prodNullTfCs [TfClause TfFalse TfAssignNull] lPTfs
  where
    -- product each condition in accTfCs with each null tf clause in pTf,
    -- and add to new list
    -- accTfCs is always rewritten
    -- if the new tf clauses is empty, directly return
    prodNullTfCs :: Route a => LinkProtoTf a -> [TfClause] -> [TfClause]
    prodNullTfCs _ [] = []
    prodNullTfCs (LinkProtoTf (Link src dst) (ProtoTf pTfCs)) [TfClause TfFalse TfAssignNull] =
      mapMaybe toNullTfClause pTfCs
      where
        toNullTfClause :: Route a => ProtoTfClause a -> Maybe TfClause
        toNullTfClause (ProtoTfClause cond rte) =
          case rte of
            Nothing -> Just $ TfClause newCond newAssign
            _       -> Nothing
          where
            newCond = simplifyCond $ appendCondVar (show dst) cond
            -- null assign
            newAssign = appendAssignVar (show src) (toTfAssign rte)
    prodNullTfCs (LinkProtoTf (Link _ dst) (ProtoTf pTfCs)) accTfCs_ =
      concatMap concatNullTfCond pTfCs
        -- concat one pTf clause with each accTf clause
      where
        concatNullTfCond :: Route a => ProtoTfClause a -> [TfClause]
        -- filter null tf clauses
        concatNullTfCond pTfC =
          case pRoute pTfC of
            Nothing -> mapMaybe (concatOneAccTf pTfC') accTfCs_
            _       -> []
            -- append dst to cond var
          where
            pTfC' = pTfC {pCond = appendCondVar (show dst) $ pCond pTfC}
            -- concat one accTf clause with one pTf clause
            concatOneAccTf ::
                 Route a => ProtoTfClause a -> TfClause -> Maybe TfClause
            concatOneAccTf pTfC_ accTfC =
              case newCond of
                TfFalse -> Nothing
                _       -> Just $ TfClause newCond (tfAssign accTfC) -- the assign is always the same null assign
              where
                newCond = simplifyCond $ TfAnd (tfCond accTfC) (pCond pTfC_)
