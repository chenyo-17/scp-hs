{-# LANGUAGE FlexibleContexts #-}

module Configurations.Router where

import           Data.List               (foldl')
import           Protocols.Base.Protocol
import           Protocols.Base.Router

-- FIXME: consider multiple protocols
-- convert a session config to session proto tf when adding it to router config
addRouterConfig ::
     (ProtocolTf a, Route (RouteType a))
  => [RouterId]
  -> [SessionProtoTf (RouteType a)]
  -> Session
  -> a
  -> [SessionProtoTf (RouteType a)]
-- assume there is no duplicate session config
-- if the ssDst is not in intRs, then assign an new id for the virtual external router
-- we collect all session tfs together because in the worst case, computing a router tf
-- would require the config of all routers to concat export and import tfs
addRouterConfig intRs ssTfs s@(Session src dir dst) a =
  if dst `notElem` intRs && dir == Export
    then let extId = getExtId dst
          in toSimpleSsProtoTf intRs (Session src Export extId, a) : ssTfs
    else toSimpleSsProtoTf intRs (s, a) : ssTfs

-- given all session tfs, compute all link tfs of one internal router with every session neighbor
-- this is used to parallelize the computation of different router tfs
toRouterTf ::
     (ProtocolTf a, Route (RouteType a))
  => RouterId
  -> [RouterId] -- used to get default session tf
  -> [RouterId]
  -> [SessionProtoTf (RouteType a)]
  -> RouterProtoTf
-- go through each session tf, if the tf's session src or dst is the router id,
-- first check whether it has been visited through its pair and added to the acc list
-- if not, find its pair and compute the link tf,
-- if there is no pair, use the default permitAll session tf
toRouterTf rId intRs nbs ssTfs =
  toRouterProtoTf $ foldl' (stepSsTf ssTfs) [] nbs
  where
    rId' =
      if rId `elem` intRs
        then rId
        else getExtId rId
    stepSsTf ::
         (ProtocolTf a, Route (RouteType a))
      => [SessionProtoTf (RouteType a)] -- to help infer that all type a should be the same type
      -> [LinkProtoTf (RouteType a)]
      -> RouterId
      -> [LinkProtoTf (RouteType a)]
    stepSsTf sTfs accLTfs nb =
      toLinkProtoTf
        (findSsTf (Session nb Export rId') sTfs)
        (findSsTf (Session rId' Import nb) sTfs)
        : accLTfs
    -- get the session tf with the same session or the default permitAll session tf
    findSsTf ::
         (ProtocolTf a, Route (RouteType a))
      => Session
      -> [SessionProtoTf (RouteType a)]
      -> SessionProtoTf (RouteType a)
    findSsTf s [] = getSimpleDefaultSsProtoTf intRs s
    findSsTf s@(Session src dir dst) (sTf@(SessionProtoTf (Session src' dir' dst') _):sTfss)
      | src == src' && dst == dst' && dir == dir' = sTf
      | otherwise = findSsTf s sTfss

-- given a list of sessions, get the nbs of a router
getNbs :: [(RouterId, RouterId)] -> RouterId -> [RouterId]
getNbs [] _ = []
getNbs ((r1, r2):rs) r
  | r1 == r = r2 : getNbs rs r
  | r2 == r = r1 : getNbs rs r
  | otherwise = getNbs rs r
