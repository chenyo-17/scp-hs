module Protocols.Base.Router where

import           Control.Parallel.Strategies
import           Data.List                   (foldl')
import           Data.Maybe                  (isNothing, mapMaybe)
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
    -- only append assign var when the router f is final!
    -- otherwise cannot extract route attribute from assign
    routerTf =
      map (\x -> TfClause (tfCond x) (appendAssignVar (show src) (tfAssign x)))
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
          concatMap mergeTfClauses pTfCs `using` parList rpar
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
-- not first use any to check whether all lTfs have null tf clauses is because
-- in practice it is rare, e.g., when consider session failure
toRouterNullTf :: Route a => RouterId -> [LinkProtoTf a] -> [TfClause]
toRouterNullTf src = prodNullTfClauses . map onlyNullLTfClauses
  where
    -- convert each link tf to a link tf where only null tf clauses remain
    -- if such link tf does not exist, return Nothing
    onlyNullLTfClauses :: Route a => LinkProtoTf a -> ProtoTf a
    onlyNullLTfClauses (LinkProtoTf (Link _ dst) pTf) = pTf'
        -- filter null tf clauses
      where
        pTf' = ProtoTf (map (appendTfCond (show dst)) $ nullClauses pTf)
        -- append link dst to the condition variables
        appendTfCond :: String -> ProtoTfClause a -> ProtoTfClause a
        appendTfCond s (ProtoTfClause cond rte) =
          ProtoTfClause (appendCondVar s cond) rte
        -- the first argument is to make the type clear
        nullClauses :: Route a => ProtoTf a -> [ProtoTfClause a]
        nullClauses pTf_ =
          filter (\(ProtoTfClause _ rte) -> isNothing rte) $ pTfClauses pTf_
    -- given a list of link tfs where each link tf only has null tf clauses,
    -- product all null tf clauses
    -- FIXME: add concurrency here, it seems it is more efficient than foldr as
    -- the list is always traversed
    prodNullTfClauses :: Route a => [ProtoTf a] -> [TfClause]
    prodNullTfClauses [] = []
    prodNullTfClauses pTfs = foldr toNullTfClause [] $ mapM pTfClauses pTfs
      -- and the condition in each clauses, simplify them,
      -- construct a new tf clause with tf assign null
      where
        toNullTfClause ::
             Route a => [ProtoTfClause a] -> [TfClause] -> [TfClause]
        toNullTfClause [] accC = accC
        toNullTfClause pTfCs accC =
          case newCond of
            TfFalse -> accC
          -- expand the null route to full attribute assign
            _       -> TfClause newCond newAssign : accC
          where
            newCond = simplifyCond $ foldr (TfAnd . pCond) TfTrue pTfCs
            newAssign = appendAssignVar (show src) (toTfAssign rte)
          -- get a dummy null route to pass to toNullTfAssign
            rte = (pRoute . head) pTfCs
