{-# LANGUAGE TypeFamilies #-}

module Protocols.Simple where

import           Data.List               (foldl', intercalate)
import           Data.Maybe              (catMaybes, fromJust)
import           Data.Word               (Word32)
import           Functions.Transfer
import           Protocols.Base.Protocol

data SimpleRoute = SimpleRoute
  { spWeight  :: Maybe SpWeightType
  , spNextHop :: Maybe Word32
  } deriving (Eq)

data SpWeightType
  = SpWeightSet Word32
  | SpWeightAdd Word32 -- add to the original weight
  deriving (Eq)

defaultSimpleRoute :: SimpleRoute
defaultSimpleRoute = SimpleRoute Nothing Nothing

data SimpleAttr
  = SimpleWeight
  | SimpleNextHop
  deriving (Eq, Show)

simpleAttrToExpr :: SimpleAttr -> String
simpleAttrToExpr SimpleWeight  = "SimpleWeight"
simpleAttrToExpr SimpleNextHop = "SimpleNextHop"

data SimpleAction
  = SimplePermit
  | SimpleDeny
  deriving (Eq, Show)

data SimpleMatch
  = MatchSimpleWeight Word32
  | MatchSimpleNextHop Word32
  deriving (Eq)

data SimpleSet
  = SetSimpleWeight Word32
  | SetSimpleNextHop Word32
  | AddSimpleWeight Word32
  deriving (Eq)

data SimpleFuncBranch = SimpleFuncBranch
  { sfAction :: SimpleAction
  , sfMatch  :: [SimpleMatch]
  , sfSet    :: [SimpleSet]
  } deriving (Eq)

newtype SimpleFunc =
  SimpleFunc [SimpleFuncBranch]
  deriving (Eq)

spAttrToString :: SimpleAttr -> String
spAttrToString attr =
  case attr of
    SimpleWeight  -> "SimpleWeight"
    SimpleNextHop -> "SimpleNextHop"

simpleFuncBranchToClause :: SimpleFuncBranch -> ProtoTfClause SimpleRoute
simpleFuncBranchToClause (SimpleFuncBranch action matches sets) =
  case action of
    SimplePermit -> ProtoTfClause conds (Just rte)
    SimpleDeny   -> ProtoTfClause conds Nothing
  where
    conds = foldr concatMatch TfTrue matches
      where
        concatMatch :: SimpleMatch -> TfCondition -> TfCondition
        concatMatch _ TfFalse = TfFalse
        concatMatch m c       = TfAnd c (simpleMatchToCond m)
    rte = foldl' stepSet defaultSimpleRoute sets
    stepSet :: SimpleRoute -> SimpleSet -> SimpleRoute
    stepSet r s =
      case s of
        SetSimpleWeight w  -> r {spWeight = Just (SpWeightSet w)}
        AddSimpleWeight w  -> r {spWeight = Just (SpWeightAdd w)}
        SetSimpleNextHop n -> r {spNextHop = Just n}

simpleMatchToCond :: SimpleMatch -> TfCondition
simpleMatchToCond m =
  case m of
    MatchSimpleNextHop nh ->
      TfCond (attrToTfExpr SimpleNextHop) TfEq (TfConst (TfInt nh))
    MatchSimpleWeight w ->
      TfCond (attrToTfExpr SimpleWeight) TfEq (TfConst (TfInt w))

simpleFuncToProtoTf :: SimpleFunc -> ProtoTf SimpleRoute
simpleFuncToProtoTf = funcToTf TfTrue
  where
    funcToTf :: TfCondition -> SimpleFunc -> ProtoTf SimpleRoute
    funcToTf TfFalse _ = ProtoTf []
    funcToTf conds (SimpleFunc []) = ProtoTf [ProtoTfClause conds Nothing]
    funcToTf conds (SimpleFunc (i:is)) =
      ProtoTf (clause : pTfClauses (funcToTf conds' (SimpleFunc is)))
      where
        ProtoTfClause c a = simpleFuncBranchToClause i
        conds' = TfAnd conds (TfNot c)
        clause = ProtoTfClause (TfAnd conds c) a

simpleStrToAttrValExpr :: SimpleAttr -> String -> TfExpr
simpleStrToAttrValExpr _ s = TfConst (TfInt (read s :: Word32))

preferFstSimpleCond :: Maybe SimpleRoute -> TfAssign -> TfAssign -> TfCondition
preferFstSimpleCond _ ass1 ass2
  | isNullAssign ass1 = TfFalse
  | isNullAssign ass2 = TfTrue
  | otherwise = TfOr largerWeight (TfAnd sameWeight smallerNextHop)
  where
    getSimpleWeight1 = fromJust $ getAssignVal (attrToTfExpr SimpleWeight) ass1
    getSimpleWeight2 = fromJust $ getAssignVal (attrToTfExpr SimpleWeight) ass2
    getNextHop :: TfAssign -> TfExpr
    getNextHop = fromJust . getAssignVal (attrToTfExpr SimpleNextHop)
    largerWeight = TfCond getSimpleWeight1 TfGt getSimpleWeight2
    sameWeight = TfCond getSimpleWeight1 TfEq getSimpleWeight2
    smallerNextHop = TfCond (getNextHop ass1) TfLt (getNextHop ass2)

simpleRouteToAssign :: Maybe SimpleRoute -> TfAssign
simpleRouteToAssign Nothing = toNullSimpleAssign
  where
    toNullSimpleAssign = TfAssign [nullWeight, nullNextHop]
      where
        nullWeight = TfAssignItem (attrToTfExpr SimpleWeight) (TfConst TfNull)
        nullNextHop = TfAssignItem (attrToTfExpr SimpleNextHop) (TfConst TfNull)
simpleRouteToAssign (Just rte) = (fromSimpleWeight . fromSimpleNh) (TfAssign [])
  where
    fromSimpleWeight :: TfAssign -> TfAssign
    fromSimpleWeight =
      case spWeight rte of
        Nothing -> addTfAssignItem weightVar weightVar
        Just (SpWeightSet w) -> addTfAssignItem weightVar (TfConst (TfInt w))
        Just (SpWeightAdd w) ->
          addTfAssignItem weightVar (TfAdd weightVar (TfConst (TfInt w)))
      where
        weightVar = attrToTfExpr SimpleWeight
    fromSimpleNh :: TfAssign -> TfAssign
    fromSimpleNh =
      case spNextHop rte of
        Nothing -> addTfAssignItem nhVar nhVar
        Just n  -> addTfAssignItem nhVar (TfConst (TfInt n))
      where
        nhVar = attrToTfExpr SimpleNextHop

updateSimpleRoute :: SimpleRoute -> SimpleRoute -> SimpleRoute
updateSimpleRoute rte1 rte2 =
  SimpleRoute
    { spWeight = updateMaybe (spWeight rte1) (spWeight rte2)
    , spNextHop = updateMaybe (spNextHop rte1) (spNextHop rte2)
    }
  where
    updateMaybe :: Maybe a -> Maybe a -> Maybe a
    updateMaybe _ (Just val2) = Just val2
    updateMaybe val1 Nothing  = val1

instance ProtoAttr SimpleAttr where
  attrToString = spAttrToString
  strToAttrValExpr = simpleStrToAttrValExpr

instance ProtocolTf SimpleFunc where
  type RouteType SimpleFunc = SimpleRoute
  toSsProtoTf (ss, sf) = SessionProtoTf ss (simpleFuncToProtoTf sf)

instance Route SimpleRoute where
  preferFstCond = preferFstSimpleCond
  toTfAssign = simpleRouteToAssign
  updateRoute = updateSimpleRoute

instance Show SpWeightType where
  show (SpWeightSet w) = show w
  show (SpWeightAdd w) = show SimpleWeight ++ " + " ++ show w

instance Show SimpleRoute where
  show r =
    (intercalate ", " . catMaybes)
      [ showJust (show SimpleWeight) (spWeight r)
      , showJust (show SimpleNextHop) (spNextHop r)
      ]
    where
      showJust :: Show a => String -> Maybe a -> Maybe String
      showJust _ Nothing  = Nothing
      showJust s (Just v) = Just $ s ++ " := " ++ show v

instance Show SimpleFuncBranch where
  show (SimpleFuncBranch action match set) =
    show action ++ ": " ++ show match ++ " -> " ++ show set

instance Show SimpleFunc where
  show (SimpleFunc branches) = unlines $ map show branches

instance Show SimpleMatch where
  show (MatchSimpleWeight w)  = "weight == " ++ show w
  show (MatchSimpleNextHop n) = "next-hop == " ++ show n

instance Show SimpleSet where
  show (SetSimpleWeight w)  = "weight := " ++ show w
  show (AddSimpleWeight w)  = "weight += " ++ show w
  show (SetSimpleNextHop n) = "next-hop := " ++ show n
