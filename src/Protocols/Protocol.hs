{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}

module Protocols.Protocol where

import           Data.Kind          (Type)
import           Data.Maybe         (catMaybes, mapMaybe)
import           Data.Word
import           Functions.Transfer

type RouterId = Word32

data SessionDir
  = Import
  | Export
  deriving (Show, Eq)

data ProtoTfClause a = ProtoTfClause
  { pCond  :: TfCondition
  , pRoute :: Maybe a -- Nothing means null route
  } deriving (Eq)

instance (Show a) => Show (ProtoTfClause a) where
  show (ProtoTfClause cond route) =
    show cond ++ " -> " ++ maybe "null" show route

-- a is an instance of Route
newtype ProtoTf a = ProtoTf
  { pTfClauses :: [ProtoTfClause a]
  } deriving (Eq)

instance (Show a) => Show (ProtoTf a) where
  show (ProtoTf clauses) = unlines $ map show clauses

-- a Import b means this is an import session at a to apply on b
-- a Export b means this is an export session at a to apply on b
data Session = Session
  { ssSrc :: RouterId
  , ssDir :: SessionDir
  , ssDst :: RouterId
  } deriving (Eq)

-- user API for creating a session
toSession :: RouterId -> SessionDir -> RouterId -> Session
toSession = Session

instance Show Session where
  show (Session src dir dst) =
    case dir of
      Import -> show src ++ "<-" ++ show dst
      Export -> show src ++ "->" ++ show dst

-- (a, b) means this is a link for a to receive b's routes
-- it is associated with a tf that chaining tf of session (b, export, a)
-- and tf of session (a, import, b)
data Link = Link
  { lkSrc :: RouterId
  , lkDst :: RouterId
  } deriving (Eq)

instance Show Link where
  show (Link src dst) = show src ++ "<-" ++ show dst

data SessionProtoTf a = SessionProtoTf
  { session :: Session
  , ssTf    :: ProtoTf a
  } deriving (Eq)

-- TODO: automatically compute indentation
instance (Show a) => Show (SessionProtoTf a) where
  show (SessionProtoTf ss tf) = "sessionTf " ++ show ss ++ ":\n" ++ show tf

data LinkProtoTf a = LinkProtoTf
  { link :: Link
  , lTf  :: ProtoTf a
  } deriving (Eq)

instance (Show a) => Show (LinkProtoTf a) where
  show (LinkProtoTf lk tf) = "linkTf " ++ show lk ++ ":\n" ++ show tf

-- from router tf, the tf is no longer proto tf
data RouterProtoTf = RouterProtoTf
  { router :: RouterId
  , rTf    :: Tf
  } deriving (Eq)

instance Show RouterProtoTf where
  show (RouterProtoTf rId tf) = "routerTf " ++ show rId ++ ":\n" ++ show tf

-- a session function is a pair of session and a,
-- where a is an instance of SessionTf
type SessionF a = (Session, a)

-- pair of session functions, the first is export tf,
-- the second is import tf
type SessionFPair a = (SessionF a, SessionF a)

class ProtoAttr a where
  -- convert each attribute to a tf expression
  toTfExpr :: a -> TfExpr

class Route a where
  -- return the condition when the first route is preferred than the second route
  -- the first argument is just use to locate the instance
  preferFstCond :: Maybe a -> TfAssign -> TfAssign -> TfCondition
  -- convert a route to a tf assign
  toTfAssign :: a -> TfAssign
  -- update first route's attributes with second route's attributes
  updateRoute :: a -> a -> a

class ProtocolTf a where
  -- declare the relationship between a protocol and its route type
  type RouteType a :: Type
  -- given a session function, convert to a session proto tf
  toSsProtoTf ::
       (Route (RouteType a)) => SessionF a -> SessionProtoTf (RouteType a)
  -- first apply toSsProtoTf, then simplify the conditions
  -- also remove false condition
  toSimpleSsProtoTf ::
       (Route (RouteType a)) => SessionF a -> SessionProtoTf (RouteType a)
  toSimpleSsProtoTf sF = sPTf {ssTf = simplePTf}
    where
      sPTf = toSsProtoTf sF
      simplePTf = ProtoTf (mapMaybe simpleCond ((pTfClauses . ssTf) sPTf))
      simpleCond (ProtoTfClause cond route) =
        case cond' of
          TfFalse -> Nothing
          _       -> Just (ProtoTfClause cond' route)
        where
          cond' = simplifyCond cond
  -- given a pair of session functions, convert to a link tf
  -- TODO: finding the right pair of session tfs
  -- is the responsibility of the config parser
  -- the goal is to compute each router's node tf concurrently with lazy evaluation
  -- e.g., a link tf is only computed when the network tf requires
  -- cannot remove null route, as it is still compared with other link tf!
  toLinkProtoTf ::
       (Route (RouteType a)) => SessionFPair a -> LinkProtoTf (RouteType a)
  toLinkProtoTf (sfe, sfi@(ssi, _)) = LinkProtoTf l pLTf
    where
      sTfe = toSimpleSsProtoTf sfe
      sTfi = toSimpleSsProtoTf sfi
      l = Link (ssSrc ssi) (ssDst ssi)
      pLTf =
        foldr concatPTfClauses (ProtoTf []) (prod2PTfs (ssTf sTfe) (ssTf sTfi))
      concatPTfClauses ::
           (Route a)
        => (ProtoTfClause a, ProtoTfClause a)
        -> ProtoTf a
        -> ProtoTf a
      -- if 2 clauses can concat, add it to the new tf
      -- otherwise, do nothing
      -- if the fist route is null route, no need to concat the second clause condition
      concatPTfClauses (newC@(ProtoTfClause _ Nothing), _) (ProtoTf pcs) =
        ProtoTf (newC : pcs)
      -- if the second route is null route, still need to concat
      concatPTfClauses (ProtoTfClause cond1 (Just rte1), ProtoTfClause cond2 rte2) pTf@(ProtoTf pcs) =
        case newCond' of
          TfFalse -> pTf
          _       -> ProtoTf (newPc : pcs)
        where
          newCond = substCond cond2 (toTfAssign rte1)
          newCond' = simplifyCond (TfAnd cond1 newCond)
          -- this is signature is here because otherwise the compiler cannot infer the type of rte1
          newRte :: Route a => a -> Maybe a -> Maybe a
          newPc = ProtoTfClause newCond' (newRte rte1 rte2)
          newRte r1 = fmap (updateRoute r1)

-- given a pair of protoTfs, product each protoTfClause pair
prod2PTfs ::
     Route a => ProtoTf a -> ProtoTf a -> [(ProtoTfClause a, ProtoTfClause a)]
prod2PTfs pTf1 pTf2 = [(c1, c2) | c1 <- pTfClauses pTf1, c2 <- pTfClauses pTf2]

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
                (prod2PTfs curPTf' restPTf)

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
    ass1 = appendAssignVar (show r1) $ maybe TfAssignNull toTfAssign rte1
    ass2 = appendAssignVar (show r2) $ maybe TfAssignNull toTfAssign rte2
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
