{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TypeFamilies            #-}

module Protocols.Protocol where

import           Data.Kind          (Type)
import           Data.Maybe         (mapMaybe)
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

-- data RouterProtoTf = RouterProtoTf
--   { router :: RouterId
--   , nTf    :: Tf
--   } deriving (Eq)
instance (Show a) => Show (LinkProtoTf a) where
  show (LinkProtoTf lk tf) = "linkTf " ++ show lk ++ ":\n" ++ show tf

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
  -- return the better route if two routes are comparable
  mergeRoute :: a -> a -> Maybe a
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
      -- if one route is null route, does not consider it
      concatPTfClauses (ProtoTfClause _ Nothing, _) pTf = pTf
      concatPTfClauses (_, ProtoTfClause _ Nothing) pTf = pTf
      concatPTfClauses (ProtoTfClause cond1 (Just rte1), ProtoTfClause _ (Just rte2)) pTf@(ProtoTf pcs) =
        case newCond of
          TfFalse -> pTf
          _       -> ProtoTf (newPc : pcs)
        where
          newCond = substCond cond1 (toTfAssign rte2)
          newRte = updateRoute rte1 rte2
          newPc = ProtoTfClause newCond (Just newRte)

-- given a pair of protoTfs, product each protoTfClause pair
prod2PTfs :: ProtoTf a -> ProtoTf a -> [(ProtoTfClause a, ProtoTfClause a)]
prod2PTfs pTf1 pTf2 = [(c1, c2) | c1 <- pTfClauses pTf1, c2 <- pTfClauses pTf2]
-- -- given a list of link tfs which has the same source router,
-- -- TODO: takes as argument all nodes' link tfs
-- -- convert them to a router tf
-- -- the link tfs cannot be empty
-- toRouterProtoTf :: [LinkProtoTf] -> RouterProtoTf
-- toRouterProtoTf lTfs@(LinkProtoTf (Link src dst) p lTf:_) =
--   RouterProtoTf src p routerTf
--   where
--     -- the router tf is computed by checking every pair of link tfs a, b
--     -- for each clause pair of a and b (ca, cb), compte the condition when one assign is better than the other
--     -- if the condition is not false, and the condition and the conditions of ca and cb to c'
--     -- the new assign is the better assign in ca and cb, and the assign should cover all setable route attributes
--     -- so ca, cb pair leads to at most 2 new clauses in the router tf
--     -- in each new clause, the variable is updated to include the router index
--     routerTf = computeRouterTf (Tf []) lTfs
--     -- given the current Tf, and a list of link tfs,
--     -- combing the head linkTf and each of the rest linkTfs and compute the new tf clauses
--     computeRouterTf :: Tf -> [LinkProtoTf] -> Tf
--     computeRouterTf accTf [] = accTf
--     computeRouterTf accTf (curLTf@(LinkProtoTf (Link _ cDst) _ curTf):restLTfs) =
--       computeRouterTf accTf' restLTfs
--       where
--         accTf' = foldr concatTfs accTf restLTfs
--         concatTfs :: LinkProtoTf -> Tf -> Tf
--         -- combine curTf and each of restTfs, and update the accTf
--         concatTfs restLTf@(LinkProtoTf (Link _ rDst) _ restTf) accTf =
--           Tf (tfClauses accTf ++ newTfClauses)
--           where
--             newTfClauses =
--               foldr (mergeLTfClauses p (cDst, rDst)) [] (prod2Tfs curTf restTf)
-- -- reverse the session direction
-- reverseDir :: SessionDir -> SessionDir
-- reverseDir dir =
--   case dir of
--     Import -> Export
--     Export -> Import
-- -- reverse the session
-- reverseSs :: Session -> Session
-- reverseSs (Session src dir dst) = Session dst (reverseDir dir) src
-- toSession :: RouterId -> SessionDir -> RouterId -> Session
-- toSession = Session
-- -- given a pair of tf clauses ca and cb, and their associated dst routers (second argument),
-- -- first convert each clause assign to protocol route ra and rb
-- -- then compute the conditions when ra is better than rb
-- -- if the condition is not false, and the condition and the conditions of ca and cb to c'
-- -- convert back the better route to assign, and construct the new clause
-- mergeLTfClauses ::
--     Protocol
--   -> (RouterId, RouterId)
--   -> (TfClause, TfClause)
--   -> [TfClause]
--   -> [TfClause]
-- mergeLTfClauses p (r1, r2) (c1, c2) accC = accC'
--   where
--     -- append _r1 and _r2 to all variable names in c1 and c2
--     c1'@(TfClause cond1' assign1') = appendClauseVar (show r1) c1
--     c2'@(TfClause cond2' assign2') = appendClauseVar (show r2) c2
--     prefer1Cond = preferFstCond p assign1' assign2'
--     prefer2Cond = preferFstCond p assign2' assign1'
--     curCond = TfAnd cond1' cond2'
--     -- TODO: if prefer1Cond is false, it is discarded
--     newC =
--       [ TfClause (TfAnd curCond prefer1Cond) assign1'
--       , TfClause (TfAnd curCond prefer2Cond) assign2'
--       ]
--     accC' = accC ++ newC
--     -- prefer1Cond = prefer
