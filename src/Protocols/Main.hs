module Main
  ( main
  ) where

import           Functions.Transfer
import           Protocols.BGP
import           Protocols.Base.Network
import           Protocols.Base.Protocol
import           Protocols.Base.Router
import           Specifications.Spec
import           Utilities.Ip

main :: IO ()
main = do
  putStrLn ""
  let ip1 = read "0.0.0.16/28" :: IpPrefix
  let ip2 = read "0.0.0.32/28" :: IpPrefix
  let pl1 = [ip1, ip2]
  let ip3 = read "0.0.0.17" :: Ip
  let ip6 = read "0.0.0.65" :: Ip
  let ip4 = read "0.0.0.64/28" :: IpPrefix
  let ip5 = read "0.0.0.128/28" :: IpPrefix
  let pl2 = [ip4, ip5]
  let cl1 = [2001]
  let cl2 = [1001]
  -- (1, 2)
  let rmItem1 = toRmItem Permit [MatchIpPrefix pl1] [SetCommunity 1001]
  let rmItem2 = toRmItem Permit [MatchCommunity cl1] [SetLocalPref 200]
  let rm1Export2 = toBgpRm [rmItem1, rmItem2]
  let rmItem7 = toRmItem Permit [MatchCommunity cl2] [SetLocalPref 100]
  let rmItem8 = toRmItem Permit [] [SetBgpNextHop ip6]
  let rm2Import1 = toBgpRm [rmItem7, rmItem8]
  -- (2, 1)
  let rmItem5 = toRmItem Permit [MatchIpPrefix pl2] [SetCommunity 2001]
  let rmItem6 = toRmItem Permit [MatchCommunity cl2] [SetLocalPref 200]
  let rm2Export1 = toBgpRm [rmItem5, rmItem6]
  let rmItem3 = toRmItem Permit [MatchCommunity cl1] [SetLocalPref 100]
  let rmItem4 = toRmItem Permit [] [SetBgpNextHop ip3]
  let rm1Import2 = toBgpRm [rmItem3, rmItem4]
  -- (3, 1)
  let rmItem9 = toRmItem Deny [MatchIpPrefix [ip1]] []
  let rmItem10 = toRmItem Permit [MatchIpPrefix [ip2]] [SetLocalPref 250]
  let rm3Export1 = toBgpRm [rmItem9, rmItem10]
  -- TODO: any deny and permit needs to be declared explicitly
  let rmItem11 = toRmItem Permit [] []
  let rm1Import3 = toBgpRm [rmItem11]
  -- (3, 2)
  let rmItem12 = toRmItem Deny [MatchIpPrefix [ip4]] []
  let rmItem13 = toRmItem Permit [MatchIpPrefix [ip5]] [SetLocalPref 50]
  let rm3Export2 = toBgpRm [rmItem12, rmItem13]
  let rmItem14 = toRmItem Permit [] []
  let rm2Import3 = toBgpRm [rmItem14]
  -- print BGP rms
  putStrLn $ "BGPrm 1->2:\n" ++ show rm1Export2
  putStrLn $ "BGPrm 2<-1:\n" ++ show rm2Import1
  putStrLn $ "BGPrm 2->1:\n" ++ show rm2Export1
  putStrLn $ "BGPrm 1<-2:\n" ++ show rm1Import2
  putStrLn $ "BGPrm 3->1:\n" ++ show rm3Export1
  putStrLn $ "BGPrm 1<-3:\n" ++ show rm1Import3
  putStrLn $ "BGPrm 3->2:\n" ++ show rm3Export2
  putStrLn $ "BGPrm 2<-3:\n" ++ show rm2Import3
  -- FIXME: this is not a good API!
  let sfExport1To2 = (toSession 1 Export 2, rm1Export2)
  let sfImport2From1 = (toSession 2 Import 1, rm2Import1)
  let sfExport2To1 = (toSession 2 Export 1, rm2Export1)
  let sfImport1From2 = (toSession 1 Import 2, rm1Import2)
  let sfExport3To1 = (toSession 3 Export 1, rm3Export1)
  let sfImport1From3 = (toSession 1 Import 3, rm1Import3)
  let sfExport3To2 = (toSession 3 Export 2, rm3Export2)
  let sfImport2From3 = (toSession 2 Import 3, rm2Import3)
  -- 12 means 1<-2
  let sfPair12 = (sfExport2To1, sfImport1From2)
  let sfPair21 = (sfExport1To2, sfImport2From1)
  let sfPair13 = (sfExport3To1, sfImport1From3)
  let sfPair23 = (sfExport3To2, sfImport2From3)
  let lPTf12 = toLinkProtoTf sfPair12
  let lPTf21 = toLinkProtoTf sfPair21
  let lPTf13 = toLinkProtoTf sfPair13
  let lPTf23 = toLinkProtoTf sfPair23
  -- -- print session tfs
  print (toSimpleSsProtoTf sfExport2To1)
  print (toSimpleSsProtoTf sfImport1From2)
  print (toSimpleSsProtoTf sfExport1To2)
  print (toSimpleSsProtoTf sfImport2From1)
  print (toSimpleSsProtoTf sfExport3To1)
  print (toSimpleSsProtoTf sfImport1From3)
  print (toSimpleSsProtoTf sfExport3To2)
  print (toSimpleSsProtoTf sfImport2From3)
  -- print link tfs
  print lPTf21
  print lPTf12
  print lPTf13
  print lPTf23
  -- print router tfs
  let rTf1 = toRouterProtoTf [lPTf12, lPTf13]
  let rTf2 = toRouterProtoTf [lPTf21, lPTf23]
  print rTf1
  print rTf2
  -- print network tfs
  let nTf = toNetProtoTf [rTf1, rTf2]
  -- print nTf
  let fpCond = toFpCond nTf
  let specs =
        [ RouterState
            (toAttrSpec BgpIpPrefix (Router 1) TfEq (Const "0.0.0.16/28"))
        ]
  putStrLn $ "Spec: \n" ++ show specs
  let specCond = toSpecCond fpCond specs
  putStrLn $ "SpecCond: \n" ++ showConds specCond
  -- putStrLn $ "FpCond:\n" ++ showConds fpCond
