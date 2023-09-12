{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeFamilyDependencies  #-}

-- {-# LANGUAGE TypeFamilies            #-}
module Protocols.Base.Protocol where

import           Data.Maybe         (mapMaybe)
import           Data.Word
import           Functions.Transfer

type RouterId = Word32

-- get the export id of an external router id
getExtId :: RouterId -> RouterId
getExtId eId = eId + 1000 -- assume there is not so many routers in total

-- get the original id, this is used to set correct from for external router's rTf
getOriginId :: RouterId -> RouterId
getOriginId nId =
  if nId > 1000
    then nId - 1000
    else nId

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

reverseDir :: SessionDir -> SessionDir
reverseDir Import = Export
reverseDir Export = Import

-- a Import b means this is an import session at a to apply on b
-- a Export b means this is an export session at a to apply on b
data Session = Session
  { ssSrc :: RouterId
  , ssDir :: SessionDir
  , ssDst :: RouterId
  } deriving (Eq)

data Action
  = Permit
  | Deny
  deriving (Show, Eq)

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
  show :: Link -> String
  show (Link src dst) = show src ++ "<-" ++ show dst

data LinkProtoTf a = LinkProtoTf
  { link :: Link
  , lTf  :: ProtoTf a
  } deriving (Eq)

instance (Show a) => Show (LinkProtoTf a) where
  show (LinkProtoTf lk tf) = "linkTf " ++ show lk ++ ":\n" ++ show tf

class Show a =>
      Route a
  where
  -- return the condition when the first route is preferred than the second route
  -- the first argument is just use to locate the instance
  preferFstCond :: Maybe a -> TfAssign -> TfAssign -> TfCondition
  -- convert a route to a tf assign
  -- the route can be null route
  toTfAssign :: Maybe a -> TfAssign
  -- update first route's attributes with second route's attributes
  updateRoute :: a -> a -> a

class Show a =>
      ProtoAttr a
  where
  -- convert each attribute to a tf expression
  attrToString :: a -> String
  attrToTfExpr :: a -> TfExpr
  attrToTfExpr = TfVar . attrToString
  -- given a string, and its attribute type, convert it a TfExpr
  -- this restricts that any route attr assign should be converted to a single cond
  -- TODO: consider complex expressions in spec, e.g., w + 10
  strToAttrValExpr :: a -> String -> TfExpr

class Show a =>
      ProtocolTf a
  where
  -- declare the relationship between a protocol and its route type
  -- type RouteType a :: Type
  -- this assures the mapping is unique
  type RouteType a = result | result -> a
  -- given a session function, convert to a session proto tf
  -- a list of internal routers is used to distinguish tf between internal and external routers
  toSsProtoTf ::
       (Route (RouteType a))
    => [RouterId]
    -> SessionF a
    -> SessionProtoTf (RouteType a)
  -- first apply toSsProtoTf, then simplify the conditions
  -- also remove false condition
  toSimpleSsProtoTf ::
       (Route (RouteType a))
    => [RouterId]
    -> SessionF a
    -> SessionProtoTf (RouteType a)
  toSimpleSsProtoTf intRs sF = sPTf {ssTf = simplePTf}
    where
      sPTf = toSsProtoTf intRs sF
      simplePTf = ProtoTf (mapMaybe simpleCond sPTfC)
        where
          sPTfC = pTfClauses (ssTf sPTf)
      simpleCond (ProtoTfClause cond route) =
        case cond' of
          TfFalse -> Nothing
          _       -> Just (ProtoTfClause cond' route)
        where
          cond' = simplifyCond cond
  getSimpleDefaultSsProtoTf ::
       (Route (RouteType a))
    => [RouterId]
    -> Session
    -> SessionProtoTf (RouteType a)
  getSimpleDefaultSsProtoTf intRs s = toSimpleSsProtoTf intRs (s, getDefault)
  getDefault :: a
  -- get the default session tf if there is no configuration

-- given a pair of session functions, and a list of internal routers,
--  convert to a link tf
-- TODO: finding the right pair of session tfs
-- is the responsibility of the config parser
-- the goal is to compute each router's node tf concurrently with lazy evaluation
-- e.g., a link tf is only computed when the network tf requires
-- cannot remove null route, as it is still compared with other link tf!
toLinkProtoTf ::
     Route a => SessionProtoTf a -> SessionProtoTf a -> LinkProtoTf a
toLinkProtoTf sTfe sTfi = LinkProtoTf l (ProtoTf pLTf)
  where
    ssi = session sTfi
    l = Link (ssSrc ssi) (ssDst ssi)
    pLTf = mapMaybe concatPTfClauses (prod2SsPTfs (ssTf sTfe) (ssTf sTfi))
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
          -> Maybe (ProtoTfClause a)
          -- if 2 clauses can concat, add it to the new tf
          -- otherwise, do nothing
          -- if the fist route is null route, no need to concat the second clause condition
        concatPTfClauses (newC@(ProtoTfClause _ Nothing), _) = Just newC
          -- if the second route is null route, still need to concat
        concatPTfClauses (ProtoTfClause cond1 (Just rte1), ProtoTfClause cond2 rte2) =
          case newCond' of
            TfFalse -> Nothing
            _       -> Just newPc
          where
            newCond = substCond cond2 (toTfAssign $ Just rte1)
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
