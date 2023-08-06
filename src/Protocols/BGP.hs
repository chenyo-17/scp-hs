{-# LANGUAGE TypeFamilies #-}
module Protocols.BGP where

import           Data.Word
import           Functions.Transfer
import           Protocols.Protocol
import           Utilities.Ip

data BgpRoute = BgpRoute
  { localPref   :: Maybe Word32
  , bgpNextHop  :: Maybe Ip
  -- only consider a single community
  , community   :: Maybe Community
  , bgpIpPrefix :: Maybe IpPrefix
  -- additional attributes for tf, guaranteed to be set
  -- they are used to include protocol internal logic,
  -- e.g., no send back
  , bgpFrom     :: RouterId
  } deriving (Eq)

-- routerId 0 is reserved for default route
defaultBgpRoute :: BgpRoute
defaultBgpRoute = BgpRoute Nothing Nothing Nothing Nothing 0

data BgpAttr
  = LocalPref
  | BgpNextHop
  | AsPath
  | BgpCommunity
  | BgpIpPrefix
  | BgpFrom
  deriving (Show, Eq)

type Community = Word32

-- action for both list and rmItem
data Action
  = Permit
  | Deny
  deriving (Show, Eq)

-- only consider exact mask length match for ip prefix
-- only consider permit
type BgpAttrListItem a = a

type BgpPlItem = BgpAttrListItem IpPrefix

type BgpClItem = BgpAttrListItem Community

type BgpAttrList a = [BgpAttrListItem a]

type BgpPl = BgpAttrList BgpPlItem

-- only consider a single community in each item
type BgpCl = BgpAttrList BgpClItem

data BgpMatch
  = MatchCommunity BgpCl
  | MatchIpPrefix BgpPl
  deriving (Eq)

data BgpSet
  = SetLocalPref Word32
  | SetBgpNextHop Ip
  | SetCommunity Community -- only consider a single community
  | SetBgpFrom RouterId
  deriving (Eq)

data RmItem = RmItem
  { rmAction :: Action
  , rmMatch  :: [BgpMatch]
  , rmSet    :: [BgpSet]
  } deriving (Eq)

-- not consider sequence number and go-to
-- new newtype, other instance Transfer BgpRm does not work
-- as BgpRm is not a concrete type
newtype BgpRm =
  BgpRm [RmItem]
  deriving (Eq)

-- convert a BgpAttr to a TfExpr
bgpAttrToExpr :: BgpAttr -> TfExpr
bgpAttrToExpr attr =
  case attr of
    LocalPref    -> TfVar "LocalPref"
    BgpNextHop   -> TfVar "BgpNextHop"
    AsPath       -> TfVar "AsPath"
    BgpCommunity -> TfVar "Community"
    BgpIpPrefix  -> TfVar "BgpIpPrefix"
    BgpFrom      -> TfVar "BgpFrom"

-- user constructor API for RmItem
toRmItem :: Action -> [BgpMatch] -> [BgpSet] -> RmItem
toRmItem = RmItem

-- user constructor API for BgpRm
toBgpRm :: [RmItem] -> BgpRm
toBgpRm = BgpRm

-- merge 2 BGP routes if they have the same IP prefix
-- the merged route is one of the 2 routes with the highest localPref
-- or the one with the shortest, or the one with a lower nextHop
mergeBgpRoute :: BgpRoute -> BgpRoute -> Maybe BgpRoute
mergeBgpRoute r1 r2
  | bgpIpPrefix r1 == bgpIpPrefix r2 = Just $ merge r1 r2
  | otherwise = Nothing
  where
    merge :: BgpRoute -> BgpRoute -> BgpRoute
    merge ra rb
      | localPref ra > localPref rb = ra
      | localPref ra < localPref rb = rb
      -- | length (asPath ra) < length (asPath rb) = ra
      -- | length (asPath ra) > length (asPath rb) = rb
      | bgpNextHop ra < bgpNextHop rb = ra
      | bgpNextHop ra > bgpNextHop rb = rb
      | otherwise = ra

-- convert a BgpRm to a ProtoTf
bgpRmToProtoTf :: BgpRm -> ProtoTf BgpRoute
bgpRmToProtoTf = rmToTf TfTrue
  where
    rmToTf :: TfCondition -> BgpRm -> ProtoTf BgpRoute
    rmToTf _ (BgpRm []) = ProtoTf []
    rmToTf conds (BgpRm (i@(RmItem act _ _):is)) =
      case act of
      -- if it is deny, the condition is recorded,
      -- but it is not added to the tf as its assign is null
        Deny   -> rmToTf conds' (BgpRm is)
        Permit -> ProtoTf $ clause : pTfClauses (rmToTf conds' (BgpRm is))
        -- for each match in item i, convert it to a TfCondition
        -- and fold them with TfAnd
        -- then negate the result and and it with old conds
        -- prepend conds to item conditions
      where
        ProtoTfClause c a = bgpItemToClause i
        clause = ProtoTfClause (TfAnd conds c) a
        negateCond = foldr (TfAnd . bgpMatchToCond) TfTrue (rmMatch i)
        conds' = TfAnd conds (TfNot negateCond)

-- convert a BgpItem to a TfClause
bgpItemToClause :: RmItem -> ProtoTfClause BgpRoute
bgpItemToClause (RmItem action matches sets) =
  case action of
    Permit -> ProtoTfClause conds (Just assigns)
    Deny   -> ProtoTfClause conds Nothing
  where
    conds = foldr (TfAnd . bgpMatchToCond) TfTrue matches
    assigns = foldr stepSet defaultBgpRoute sets
    -- accumulate all BgpSets into a single BgpRoute
    stepSet :: BgpSet -> BgpRoute -> BgpRoute
    stepSet s r =
      case s of
        SetLocalPref lp  -> r {localPref = Just lp}
        SetBgpNextHop nh -> r {bgpNextHop = Just nh}
        SetCommunity c   -> r {community = Just c}
        SetBgpFrom f     -> r {bgpFrom = f}

-- -- convert a BgpRoute to a TfAssign
bgpRouteToAssign :: BgpRoute -> TfAssign
bgpRouteToAssign rte =
  (fromBgpFrom
     . fromLocalPref
     . fromBgpNextHop
     . fromBgpCommunity
     . fromIpPrefix)
    (TfAssign [])
  where
    -- TODO: this is ugly
    fromIpPrefix :: TfAssign -> TfAssign
    -- assume ip prefix is never reset
    fromIpPrefix = addTfAssignItem ipPrefixVar (keepOldVar ipPrefixVar)
      where
        ipPrefixVar = bgpAttrToExpr BgpIpPrefix
    fromBgpCommunity :: TfAssign -> TfAssign
    fromBgpCommunity ass =
      case community rte of
        Nothing   -> addTfAssignItem communityVar (keepOldVar communityVar) ass
          -- keep old value
        Just comm -> addTfAssignItem communityVar (TfConst (TfInt comm)) ass
      where
        communityVar = bgpAttrToExpr BgpCommunity
    fromBgpNextHop :: TfAssign -> TfAssign
    fromBgpNextHop ass =
      case bgpNextHop rte of
        Nothing -> addTfAssignItem nextHopVar (keepOldVar nextHopVar) ass
        Just nh -> addTfAssignItem nextHopVar (TfConst (TfInt (fromIpw nh))) ass
      where
        nextHopVar = bgpAttrToExpr BgpNextHop
    fromLocalPref :: TfAssign -> TfAssign
    fromLocalPref ass =
      case localPref rte of
        Nothing -> addTfAssignItem localPrefVar (keepOldVar localPrefVar) ass
        Just lp -> addTfAssignItem localPrefVar (TfConst (TfInt lp)) ass
      where
        localPrefVar = bgpAttrToExpr LocalPref
    fromBgpFrom :: TfAssign -> TfAssign
    -- bgp from must be already set
    fromBgpFrom = addTfAssignItem bgpFromVar (TfConst (TfInt fr))
      where
        bgpFromVar = bgpAttrToExpr BgpFrom
        fr = bgpFrom rte

-- convert a BgpMatch to a TfCondition
bgpMatchToCond :: BgpMatch -> TfCondition
bgpMatchToCond m =
  case m of
    -- the route ip prefix belongs to the range of the ip prefix list
    -- for k prefix list items, there are 2 * k conditions to be Or'ed
    MatchIpPrefix pl  -> foldr (TfOr . plItemToCond) TfFalse pl
    MatchCommunity cl -> foldr (TfOr . clItemToCond) TfFalse cl

-- convert a BgpPlItem to a TfCondition
plItemToCond :: BgpPlItem -> TfCondition
plItemToCond pli = TfAnd geIpLow leIpHiw
  where
    (ipLow, ipHiw) = toIpRangew pli
    geIpLow = TfCond (bgpAttrToExpr BgpIpPrefix) TfGe (TfConst (TfInt ipLow))
    leIpHiw = TfCond (bgpAttrToExpr BgpIpPrefix) TfLe (TfConst (TfInt ipHiw))

-- convert a BgpClItem to a TfCondition
-- each item only contains a single community
clItemToCond :: BgpClItem -> TfCondition
clItemToCond ci = TfCond (bgpAttrToExpr BgpCommunity) TfEq (TfConst (TfInt ci))

-- add a setBgpFrom in each item of a rm if it is an import session
setBgpFrom :: RouterId -> BgpRm -> BgpRm
setBgpFrom from (BgpRm is) = BgpRm (map addFrom is)
  where
    addFrom :: RmItem -> RmItem
    addFrom (RmItem action matches sets) =
      case action of
      -- only set BgpFrom if it's a Permit
        Permit -> RmItem action matches (SetBgpFrom from : sets)
        Deny   -> RmItem action matches sets

instance ProtocolTf BgpRm where
  type RouteType BgpRm = BgpRoute
  toSsProtoTf (ss@(Session _ sDir sDst), rm) = sTf
    -- if it is an import session, set BgpFrom in every item
    -- TODO: also and additional match, e.g., matchSession
    where
      rm' =
        if sDir == Import
          then setBgpFrom sDst rm
          else rm
      sTf = SessionProtoTf ss (bgpRmToProtoTf rm')

instance Route BgpRoute where
  mergeRoute = mergeBgpRoute
  toTfAssign = bgpRouteToAssign

instance Show BgpMatch where
  show (MatchCommunity cl) = "match community " ++ show cl
  show (MatchIpPrefix pl)  = "match ip-prefix " ++ show pl

instance Show BgpSet where
  show (SetLocalPref lp)  = "set local-pref " ++ show lp
  show (SetBgpNextHop nh) = "set next-hop " ++ show nh
  show (SetCommunity c)   = "set community " ++ show c
  show (SetBgpFrom f)     = "set from " ++ show f

instance Show RmItem where
  show (RmItem action match set) =
    show action ++ ": " ++ show match ++ " -> " ++ show set

instance Show BgpRm where
  show (BgpRm rmItems) = unlines $ map show rmItems

instance Show BgpRoute where
  show r =
    "ip-prefix: "
      ++ show (bgpIpPrefix r)
      ++ ", next-hop: "
      ++ show (bgpNextHop r)
      ++ ", community: "
      ++ show (community r)
      ++ ", local-pref: "
      ++ show (localPref r)
      ++ ", from: "
      ++ show (bgpFrom r)
