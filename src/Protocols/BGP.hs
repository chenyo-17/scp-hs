module Protocols.BGP where

import           Data.Word
import           Functions.Transfer
import           Protocols.Protocol
import           Utilities.Ip

data BgpRoute = BgpRoute
  { localPref   :: Word32
  , bgpNextHop  :: Ip
  , asPath      :: [AsN]
  -- only consider a single community
  , community   :: Community
  , bgpIpPrefix :: IpPrefix
  -- additional attributes for tf
  , bgpFrom     :: RouterId
  } deriving (Show, Eq)

data BgpAttr
  = LocalPref
  | BgpNextHop
  | AsPath
  | BgpCommunity
  | BgpIpPrefix
  | BgpFrom
  deriving (Show, Eq)

type AsN = Word16

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
      | length (asPath ra) < length (asPath rb) = ra
      | length (asPath ra) > length (asPath rb) = rb
      | bgpNextHop ra < bgpNextHop rb = ra
      | bgpNextHop ra > bgpNextHop rb = rb
      | otherwise = ra

-- convert a BgpRm to a Tf
bgpRmToTf :: BgpRm -> Tf
bgpRmToTf = rmToTf TfTrue
  where
    rmToTf :: TfCondition -> BgpRm -> Tf
    rmToTf _ (BgpRm []) = Tf []
    rmToTf conds (BgpRm (i@(RmItem act _ _):is)) = case act of 
      -- if it is deny, the condition is recorded, 
      -- but it is not added to the tf as its assign is null
      Deny -> rmToTf conds' (BgpRm is)
      Permit -> Tf $ clause : tfClauses (rmToTf conds' (BgpRm is))
      where
        -- for each match in item i, convert it to a TfCondition
        -- and fold them with TfAnd
        -- then negate the result and and it with old conds
        -- prepend conds to item conditions
        TfClause c a = bgpItemToClause i
        clause = TfClause (TfAnd conds c) a
        negateCond = foldr (TfAnd . bgpMatchToCond) TfTrue (rmMatch i)
        conds' = TfAnd conds (TfNot negateCond)

-- convert a BgpItem to a TfClause
bgpItemToClause :: RmItem -> TfClause
bgpItemToClause (RmItem action matches sets) =
  case action of
    Permit -> TfClause conds assigns
    Deny   -> TfClause conds TfAssignNull
  where
    conds = foldr (TfAnd . bgpMatchToCond) TfTrue matches
    assigns = foldr stepSet (TfAssign []) sets
    -- accumulate all BgpSets into a single TfAssign
    stepSet :: BgpSet -> TfAssign -> TfAssign
    stepSet s = combine (bgpSetToAssign s)
        -- combine 2 TfAssigns into a single TfAssign
      where
        combine :: TfAssign -> TfAssign -> TfAssign
        combine (TfAssign a1) (TfAssign a2) = TfAssign (a1 ++ a2)
        combine TfAssignNull _              = TfAssignNull
        combine _ TfAssignNull              = TfAssignNull

-- convert a BgpMatch to a TfCondition
bgpMatchToCond :: BgpMatch -> TfCondition
bgpMatchToCond m =
  case m of
    -- the route ip prefix belongs to the range of the ip prefix list
    -- for k prefix list items, there are 2 * k conditions to be Or'ed
    MatchIpPrefix pl  -> foldr (TfOr . plItemToCond) TfFalse pl
    MatchCommunity cl -> foldr (TfOr . clItemToCond) TfFalse cl

-- convert a BgpSet to a TfAssign
bgpSetToAssign :: BgpSet -> TfAssign
bgpSetToAssign s =
  case s of
    SetLocalPref lp ->
      TfAssign [TfAssignItem (bgpAttrToExpr LocalPref) (TfConst (TfInt lp))]
    SetBgpNextHop nh ->
      TfAssign [TfAssignItem (bgpAttrToExpr BgpNextHop) (TfConst (TfInt nhw))]
      where nhw = fromIpw nh
    SetCommunity c ->
      TfAssign [TfAssignItem (bgpAttrToExpr BgpCommunity) (TfConst (TfInt c))]
    SetBgpFrom f ->
      TfAssign [TfAssignItem (bgpAttrToExpr BgpFrom) (TfConst (TfInt f))]

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

-- add a setBgpFrom in each BgpRmItem
setBgpFrom :: RouterId -> BgpRm -> BgpRm
setBgpFrom from (BgpRm is) = BgpRm (map addFrom is)
  where
    addFrom :: RmItem -> RmItem
    addFrom (RmItem action matches sets) =
      case action of
      -- only set BgpFrom if it's a Permit
        Permit -> RmItem action matches (SetBgpFrom from : sets)
        Deny   -> RmItem action matches sets

instance SessionTf BgpRm where
  addSessionTf ss@(Session _ sDir sDst) rm ssTfs = sTf : ssTfs
    -- if it is an import session, set BgpFrom in every item
    -- TODO: also and additional match, e.g., matchSession
    where
      rm' =
        if sDir == Import
          then setBgpFrom sDst rm
          else rm
      sTf = SessionProtoTf ss BGP (toSimpleTf rm')

instance Route BgpRoute where
  mergeRoute = mergeBgpRoute

instance Transfer BgpRm where
  toTf = bgpRmToTf

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
