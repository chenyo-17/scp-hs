{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeFamilies            #-}

module Protocols.Base.Protocol where

import           Data.Kind          (Type)
import           Data.Maybe         (mapMaybe)
import           Data.Word
import           Functions.Transfer

type RouterId = Word32

data ProtoTfClause a = ProtoTfClause
  { pCond  :: TfCondition
  , pRoute :: Maybe a -- Nothing means null route
  } deriving (Eq)

-- a is an instance of Route
newtype ProtoTf a = ProtoTf
  { pTfClauses :: [ProtoTfClause a]
  } deriving (Eq)

data SessionDir
  = Import
  | Export
  deriving (Show, Eq)

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

data SessionProtoTf a = SessionProtoTf
  { session :: Session
  , ssTf    :: ProtoTf a
  } deriving (Eq)

-- a session function is a pair of session and a,
-- where a is an instance of SessionTf
type SessionF a = (Session, a)

-- pair of session functions, the first is export tf,
-- the second is import tf
type SessionFPair a = (SessionF a, SessionF a)

-- (a, b) means this is a link for a to receive b's routes
-- it is associated with a tf that chaining tf of session (b, export, a)
-- and tf of session (a, import, b)
data Link = Link
  { lkSrc :: RouterId
  , lkDst :: RouterId
  } deriving (Eq)

instance Show Link where
  show (Link src dst) = show src ++ "<-" ++ show dst

data LinkProtoTf a = LinkProtoTf
  { link :: Link
  , lTf  :: ProtoTf a
  } deriving (Eq)

instance (Show a) => Show (LinkProtoTf a) where
  show (LinkProtoTf lk tf) = "linkTf " ++ show lk ++ ":\n" ++ show tf

class Route a where
  -- return the condition when the first route is preferred than the second route
  -- the first argument is just use to locate the instance
  preferFstCond :: Maybe a -> TfAssign -> TfAssign -> TfCondition
  -- convert a route to a tf assign
  toTfAssign :: a -> TfAssign
  -- update first route's attributes with second route's attributes
  updateRoute :: a -> a -> a
  -- convert a null route to a tf assign
  toNullTfAssign :: Maybe a -> TfAssign

class ProtoAttr a where
  -- convert each attribute to a tf expression
  toTfExpr :: a -> TfExpr

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
        foldr
          concatPTfClauses
          (ProtoTf [])
          (prod2SsPTfs (ssTf sTfe) (ssTf sTfi))
            -- product 2 session proto tf clauses, but if the first clause is null route,
            -- then pair it with Nothing
        where
          prod2SsPTfs ::
               Route a
            => ProtoTf a
            -> ProtoTf a
            -> [(ProtoTfClause a, ProtoTfClause a)]
          prod2SsPTfs (ProtoTf []) _ = [] -- cannot be empty in practice
          prod2SsPTfs (ProtoTf (pTfC1:pTfC1s)) pTf2@(ProtoTf pTfC2s) =
            case pRoute pTfC1 of
              Nothing ->
                (pTfC1, ProtoTfClause TfTrue Nothing) -- dummy clause, not checked in concatPTfClauses
                  : prod2SsPTfs (ProtoTf pTfC1s) pTf2
              _ -> map (pTfC1, ) pTfC2s ++ prod2SsPTfs (ProtoTf pTfC1s) pTf2
            -- concat 2 session proto tf clauses and add it to the new tf
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

instance (Show a) => Show (ProtoTfClause a) where
  show (ProtoTfClause cond route) =
    show cond ++ " -> " ++ maybe "null" show route

instance (Show a) => Show (ProtoTf a) where
  show (ProtoTf clauses) = unlines $ map show clauses

-- TODO: automatically compute indentation
instance (Show a) => Show (SessionProtoTf a) where
  show (SessionProtoTf ss tf) = "sessionTf " ++ show ss ++ ":\n" ++ show tf
