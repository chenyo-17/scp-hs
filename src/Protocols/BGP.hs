{-# LANGUAGE TypeFamilies #-}

module Protocols.BGP where

import           Data.List               (foldl', intercalate)
import           Data.Maybe
import           Data.Word
import           Functions.Transfer
import           Protocols.Base.Protocol
import           Utilities.Ip

data BgpRoute = BgpRoute
  { localPref   :: Maybe Word32
  , bgpNextHop  :: Maybe Ip
  -- only consider a single community
  , community   :: Maybe BgpCommunity
  , bgpIpPrefix :: Maybe IpPrefix
  -- additional attributes for tf, guaranteed to be set
  -- they are used to include protocol internal logic,
  -- e.g., no send back
  , bgpFrom     :: Maybe RouterId
  , bgpOrigin   :: Maybe Word32
  } deriving (Eq)

-- routerId 0 is reserved for default route
defaultBgpRoute :: BgpRoute
defaultBgpRoute = BgpRoute Nothing Nothing Nothing Nothing Nothing Nothing

data BgpAttr
  = LocalPref
  | BgpNextHop
  | Community
  | BgpIpPrefix
  | BgpFrom
  -- | BgpOrigin
  deriving (Show, Eq)

type BgpCommunity = Word32

data BgpAction
  = BgpPermit
  | BgpDeny
  deriving (Show, Eq)

-- only consider exact mask length match for ip prefix
-- only consider permit
type BgpAttrListItem a = a

type BgpPlItem = BgpAttrListItem IpPrefix

type BgpClItem = BgpAttrListItem BgpCommunity

type BgpAttrList a = [BgpAttrListItem a]

type BgpPl = BgpAttrList BgpPlItem

-- only consider a single community in each item
type BgpCl = BgpAttrList BgpClItem

data BgpMatch
  = MatchCommunity BgpCl
  | MatchBgpIpPrefix BgpPl -- only consider exact match
  | MatchBgpNextHop BgpPl
  | MatchBgpFrom RouterId
  deriving (Eq)

data BgpSet
  = SetLocalPref Word32
  | SetBgpNextHop Ip
  | SetCommunity BgpCommunity -- only consider a single community
  | SetBgpFrom RouterId
  | SetIpPrefix IpPrefix
  deriving (Eq)

data RmItem = RmItem
  { rmAction :: BgpAction
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
bgpAttrToString :: BgpAttr -> String
bgpAttrToString attr =
  case attr of
    LocalPref   -> "LocalPref"
    BgpNextHop  -> "BgpNextHop"
    Community   -> "Community"
    BgpIpPrefix -> "BgpIpPrefix"
    BgpFrom     -> "BgpFrom"
    -- BgpOrigin   -> "BgpOrigin"

-- user constructor API for RmItem
toRmItem :: BgpAction -> [BgpMatch] -> [BgpSet] -> RmItem
toRmItem = RmItem

-- user constructor API for BgpRm
toBgpRm :: [RmItem] -> BgpRm
toBgpRm = BgpRm

-- convert a BgpRm to a ProtoTf
bgpRmToProtoTf :: BgpRm -> ProtoTf BgpRoute
bgpRmToProtoTf = rmToTf TfTrue
  where
    rmToTf :: TfCondition -> BgpRm -> ProtoTf BgpRoute
    -- if the condition becomes false, ignore the rest of the items
    rmToTf TfFalse _ = ProtoTf []
    -- add a default deny clause
    rmToTf conds (BgpRm []) = ProtoTf [ProtoTfClause conds Nothing]
    rmToTf conds (BgpRm (i:is)) =
      ProtoTf (clause : pTfClauses (rmToTf conds' (BgpRm is)))
        -- for each match in item i, convert it to a TfCondition
        -- and fold them with TfAnd
        -- then negate the result and and it with old conds
        -- prepend conds to item conditions
      where
        ProtoTfClause c a = bgpItemToClause i
        conds' = TfAnd conds (TfNot c)
        clause = ProtoTfClause (TfAnd conds c) a

-- convert a BgpItem to a TfClause
bgpItemToClause :: RmItem -> ProtoTfClause BgpRoute
bgpItemToClause (RmItem action matches sets) =
  case action of
    BgpPermit -> ProtoTfClause conds (Just rte)
    BgpDeny   -> ProtoTfClause conds Nothing
  where
    conds = foldr concatMatch TfTrue matches
      where
        concatMatch :: BgpMatch -> TfCondition -> TfCondition
        concatMatch _ TfFalse = TfFalse
        concatMatch m c       = TfAnd (bgpMatchToCond m) c
    -- the sets is always consumed
    rte = foldl' stepSet defaultBgpRoute sets
    -- accumulate all BgpSets into a single BgpRoute
    stepSet :: BgpRoute -> BgpSet -> BgpRoute
    stepSet r s =
      case s of
        SetLocalPref lp   -> r {localPref = Just lp}
        SetBgpNextHop nh  -> r {bgpNextHop = Just nh}
        SetCommunity comm -> r {community = Just comm}
        SetBgpFrom fr     -> r {bgpFrom = Just fr}
        SetIpPrefix ipP   -> r {bgpIpPrefix = Just ipP}

-- given a attribute type, and a string, parse it to a TfExpr
-- FIXME: this is a bit duplicate with bgpRouteToAssign, it is just used to parse spec
bgpStrToAttrValExpr :: BgpAttr -> String -> TfExpr
bgpStrToAttrValExpr LocalPref lp = (TfConst . TfInt) (read lp :: Word32)
bgpStrToAttrValExpr BgpNextHop nh = (TfConst . TfInt . fromIpw) (read nh :: Ip)
bgpStrToAttrValExpr Community c = (TfConst . TfInt) (read c :: Word32)
bgpStrToAttrValExpr BgpIpPrefix ipP =
  (TfConst . TfInt . fst . toIpRangew) (read ipP :: IpPrefix)
bgpStrToAttrValExpr BgpFrom fr = (TfConst . TfInt) (read fr :: Word32)

-- bgpStrToAttrValExpr BgpOrigin o = (TfConst . TfInt) (read o :: Word32)
-- convert a BgpRoute to a TfAssign
-- all attributes must be covered,
-- if some attributes are not set, set it to the attribute var
bgpRouteToAssign :: Maybe BgpRoute -> TfAssign
bgpRouteToAssign Nothing = toNullBgpAssign
  where
    -- convert a null BgpRoute to a TfAssign
    toNullBgpAssign :: TfAssign
    toNullBgpAssign =
      TfAssign
        [ nullLocalPref
        , nullIpPrefix
        , nullFrom
        , nullNextHop
        , nullCommunity
        -- , nullOrigin
        ]
      where
        nullLocalPref = TfAssignItem (attrToTfExpr LocalPref) (TfConst TfNull)
        nullIpPrefix = TfAssignItem (attrToTfExpr BgpIpPrefix) (TfConst TfNull)
        nullFrom = TfAssignItem (attrToTfExpr BgpFrom) (TfConst TfNull)
        nullNextHop = TfAssignItem (attrToTfExpr BgpNextHop) (TfConst TfNull)
        nullCommunity = TfAssignItem (attrToTfExpr Community) (TfConst TfNull)
        -- nullOrigin = TfAssignItem (attrToTfExpr BgpOrigin) (TfConst TfNull)
bgpRouteToAssign (Just rte) =
  (fromBgpFrom
     . fromLocalPref
     . fromBgpNextHop
     . fromBgpCommunity
     . fromIpPrefix)
    --  . fromBgpOrigin)
    (TfAssign [])
  where
    -- FIXME: this is ugly
    fromIpPrefix :: TfAssign -> TfAssign
    fromIpPrefix =
      case bgpIpPrefix rte of
        Nothing -> addTfAssignItem ipPrefixVar ipPrefixVar
        Just ipP
          -- an ip prefix set is to a single value
         ->
          addTfAssignItem ipPrefixVar (TfConst (TfInt ((fst . toIpRangew) ipP)))
      where
        ipPrefixVar = attrToTfExpr BgpIpPrefix
    fromBgpCommunity :: TfAssign -> TfAssign
    fromBgpCommunity ass =
      case community rte of
        Nothing   -> addTfAssignItem communityVar communityVar ass
          -- keep old value
        Just comm -> addTfAssignItem communityVar (TfConst (TfInt comm)) ass
      where
        communityVar = attrToTfExpr Community
    fromBgpNextHop :: TfAssign -> TfAssign
    fromBgpNextHop ass =
      case bgpNextHop rte of
        Nothing -> addTfAssignItem nextHopVar nextHopVar ass
        Just nh -> addTfAssignItem nextHopVar (TfConst (TfInt (fromIpw nh))) ass
      where
        nextHopVar = attrToTfExpr BgpNextHop
    fromLocalPref :: TfAssign -> TfAssign
    fromLocalPref ass =
      case localPref rte of
        Nothing -> addTfAssignItem localPrefVar localPrefVar ass
        Just lp -> addTfAssignItem localPrefVar (TfConst (TfInt lp)) ass
      where
        localPrefVar = attrToTfExpr LocalPref
    fromBgpFrom :: TfAssign -> TfAssign
    fromBgpFrom ass =
      case bgpFrom rte of
        Nothing -> addTfAssignItem bgpFromVar bgpFromVar ass
        Just fr -> addTfAssignItem bgpFromVar (TfConst (TfInt fr)) ass
      where
        bgpFromVar = attrToTfExpr BgpFrom
    -- fromBgpOrigin :: TfAssign -> TfAssign
    -- fromBgpOrigin ass =
    --   case bgpOrigin rte of
    --     Nothing -> addTfAssignItem bgpOriginVar bgpOriginVar ass
    --     Just o  -> addTfAssignItem bgpOriginVar (TfConst (TfInt o)) ass
    --   where
    --     bgpOriginVar = attrToTfExpr BgpOrigin

-- update first route's attributes with the second route
-- if they update the same attribute
updateBgpRoute :: BgpRoute -> BgpRoute -> BgpRoute
updateBgpRoute rte1 rte2 =
  BgpRoute
    -- if the second route is not Nothing, return it, otherwise, return the first
    { localPref = updateMaybe (localPref rte1) (localPref rte2)
    , bgpNextHop = updateMaybe (bgpNextHop rte1) (bgpNextHop rte2)
    , community = updateMaybe (community rte1) (community rte2)
    , bgpIpPrefix = updateMaybe (bgpIpPrefix rte1) (bgpIpPrefix rte2)
    , bgpFrom = updateMaybe (bgpFrom rte1) (bgpFrom rte2)
    , bgpOrigin = updateMaybe (bgpOrigin rte1) (bgpOrigin rte2)
    }
  where
    updateMaybe :: Maybe a -> Maybe a -> Maybe a
    updateMaybe _ (Just val2) = Just val2
    updateMaybe val1 Nothing  = val1

-- convert a BgpMatch to a TfCondition
bgpMatchToCond :: BgpMatch -> TfCondition
bgpMatchToCond m =
  case m of
    -- the route ip prefix belongs to the range of the ip prefix list
    -- for k prefix list items, there are 2 * k conditions to be Or'ed
    MatchBgpIpPrefix pl -> foldr concatPlItem TfFalse pl
      where concatPlItem :: BgpPlItem -> TfCondition -> TfCondition
            concatPlItem _ TfTrue = TfTrue
            concatPlItem pli cond = TfOr (ipPlItemToCond pli) cond
    MatchBgpNextHop pl -> foldr concatPlItem TfFalse pl
      where concatPlItem :: BgpPlItem -> TfCondition -> TfCondition
            concatPlItem _ TfTrue = TfTrue
            concatPlItem pli cond = TfOr (nhPlItemToCond pli) cond
    MatchCommunity cl -> foldr concatClItem TfFalse cl
      where concatClItem :: BgpClItem -> TfCondition -> TfCondition
            concatClItem _ TfTrue = TfTrue
            concatClItem ci cond  = TfOr (clItemToCond ci) cond
    MatchBgpFrom fr -> TfCond (attrToTfExpr BgpFrom) TfEq (TfConst (TfInt fr))

-- convert a BgpPlItem to a TfCondition
-- for next hop, the condition is a range,
nhPlItemToCond :: BgpPlItem -> TfCondition
nhPlItemToCond pli = TfAnd geIpLow leIpHiw
  where
    (ipLow, ipHiw) = toIpRangew pli
    geIpLow = TfCond (attrToTfExpr BgpIpPrefix) TfGe (TfConst (TfInt ipLow))
    leIpHiw = TfCond (attrToTfExpr BgpIpPrefix) TfLe (TfConst (TfInt ipHiw))

-- for ip prefix, the condition is a single value (ipLow), as now only consider exact match
-- so that the set ip prefix can be converted to a single equality condition
ipPlItemToCond :: BgpPlItem -> TfCondition
ipPlItemToCond pli =
  TfCond (attrToTfExpr BgpIpPrefix) TfEq (TfConst (TfInt ipLow))
  where
    (ipLow, _) = toIpRangew pli

-- convert a BgpClItem to a TfCondition
-- each item only contains a single community
clItemToCond :: BgpClItem -> TfCondition
clItemToCond ci = TfCond (attrToTfExpr Community) TfEq (TfConst (TfInt ci))

-- add a setBgpFrom in each item of a rm if it is an import session
setBgpFrom :: RouterId -> BgpRm -> BgpRm
setBgpFrom from (BgpRm is) = BgpRm (map addFrom is)
  where
    addFrom :: RmItem -> RmItem
    addFrom (RmItem action matches sets) =
      case action of
      -- only set BgpFrom if it's a Permit
        BgpPermit -> RmItem action matches (SetBgpFrom from : sets)
        BgpDeny   -> RmItem action matches sets

-- prepend an item that matches the route's BgpFrom with a deny action
denySameBgpFrom :: RouterId -> BgpRm -> BgpRm
denySameBgpFrom from (BgpRm is) = BgpRm (denyItem : is)
  where
    denyItem :: RmItem
    denyItem = RmItem BgpDeny [MatchBgpFrom from] []

-- return the conditions when the first route assign is preferred
-- the passed assigns have been added with router ids
-- the first argument is dummy, just to locate the instance
-- FIXME: the conversion from assign to attribute is not efficient
preferFstBgpCond :: Maybe BgpRoute -> TfAssign -> TfAssign -> TfCondition
preferFstBgpCond _ ass1 ass2
  -- if the first assign is null route, it can never be preferred
  | isNullAssign ass1 = TfFalse
  -- if the second assign is null route, the first not null assign is always preferred
  | isNullAssign ass2 = TfTrue
  -- if two routes have different ip prefix, always prefer first
  -- here is to add default protocol rules or conditions that require node id
  -- e.g., no send back
  | otherwise =
    TfOr
      notSameIpPrefix -- not same prefix, and the current prefix is not null
      (TfAnd
         sameIpPrefix
         (TfOr largerLocalPref (TfAnd sameLocalPref largerFrom)))
  -- prefer lower lp, and then larger from (to prefer external route later)
  where
    sameIpPrefix = TfCond (getIpPrefix ass1) TfEq (getIpPrefix ass2)
    notSameIpPrefix =
      TfAnd
        (TfNot sameIpPrefix)
        (TfCond (getIpPrefix ass1) TfNe (TfConst TfNull))
    largerLocalPref = TfCond getLocalPref1 TfGt getLocalPref2
    sameLocalPref = TfCond getLocalPref1 TfEq getLocalPref2
    largerFrom = TfCond (getFrom ass1) TfGt (getFrom ass2)
    getIpPrefix :: TfAssign -> TfExpr
      -- null case is already handled, here it must be just
    getIpPrefix = fromJust . getAssignVal (attrToTfExpr BgpIpPrefix)
    getLocalPref1 :: TfExpr
    getLocalPref1 = fromJust $ getAssignVal (attrToTfExpr LocalPref) ass1
    getLocalPref2 :: TfExpr
    getLocalPref2 = fromJust $ getAssignVal (attrToTfExpr LocalPref) ass2
    getFrom :: TfAssign -> TfExpr
    getFrom = fromJust . getAssignVal (attrToTfExpr BgpFrom)

instance ProtoAttr BgpAttr where
  attrToString = bgpAttrToString
  strToAttrValExpr = bgpStrToAttrValExpr

instance ProtocolTf BgpRm where
  type RouteType BgpRm = BgpRoute
  toSsProtoTf intRs (ss@(Session sSrc sDir sDst), rm) = sTf
    -- if it is an import session, set BgpFrom in every item
    -- and prepend matchBgpFrom -> drop to the first item
    -- TODO: also and additional match, e.g., matchSession
    -- TODO: think about how to let Protocol handle additional attribute
    where
      rm' =
        if sDir == Import
          -- if the dst is an internal router, deny the same bgp from to avoid loop
          then if sDst `elem` intRs
                 then denySameBgpFrom (getOriginId sSrc) (setBgpFrom sDst rm)
                 else setBgpFrom sDst rm
          else rm
      -- only deny same BgpFrom if it is an internal router
      sTf = SessionProtoTf ss (bgpRmToProtoTf rm')
  getDefault  = toBgpRm [toRmItem BgpPermit [] []]

instance Route BgpRoute
  -- FIXME: some methods are required only because cannot know the attribute
  -- during the runtime
                        where
  preferFstCond = preferFstBgpCond
  toTfAssign = bgpRouteToAssign
  updateRoute = updateBgpRoute

instance Show BgpMatch where
  show (MatchCommunity cl)   = "match community " ++ show cl
  show (MatchBgpIpPrefix pl) = "match ip-prefix " ++ show pl
  show (MatchBgpNextHop nh)  = "match next-hop " ++ show nh
  show (MatchBgpFrom fr)     = "match from " ++ show fr

instance Show BgpSet where
  show (SetLocalPref lp)  = "set local-pref " ++ show lp
  show (SetBgpNextHop nh) = "set next-hop " ++ show nh
  show (SetCommunity 0)   = "del community"
  show (SetCommunity c)   = "set community " ++ show c
  show (SetBgpFrom f)     = "set from " ++ show f
  show (SetIpPrefix ipP)  = "set ip-prefix " ++ show ipP

instance Show RmItem where
  show (RmItem action match set) =
    show action ++ ": " ++ show match ++ " -> " ++ show set

instance Show BgpRm where
  show (BgpRm rmItems) = unlines $ map show rmItems

instance Show BgpRoute
  -- only show the attributes that are not Nothing
                                                   where
  show r =
    (intercalate ", " . catMaybes)
      [ showJust (show BgpIpPrefix) (bgpIpPrefix r)
      , showJust (show BgpNextHop) (bgpNextHop r)
      , showJust (show Community) (community r)
      , showJust (show LocalPref) (localPref r)
      , showJust (show BgpFrom) (bgpFrom r)
      -- , showJust (show BgpOrigin) (bgpOrigin r)
      ]
    where
      showJust :: Show a => String -> Maybe a -> Maybe String
      showJust _ Nothing  = Nothing
      showJust s (Just v) = Just $ s ++ " := " ++ show v
