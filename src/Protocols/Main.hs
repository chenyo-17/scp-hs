module Main
  ( main
  ) where

import           Functions.Solver        (simplifyCondWithSolver)
import           Functions.Transfer
import           Protocols.BGP
import           Protocols.Base.Network
import           Protocols.Base.Protocol
import           Protocols.Base.Router
import           Protocols.Simple
import           Specifications.Spec
import           System.Environment
import           Utilities.Io
import           Utilities.Ip

mainSimple :: FilePath -> IO ()
mainSimple condOutPath = do
  putStrLn ""
  -- all export funcs are empty
  -- TODO: automate this
  let intRouters = [0, 1, 2]
  let sf3Export0 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
  let sf0Export1 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
  let sf1Export0 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
  let sf1Export2 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
  let sf2Export1 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
  let sf4Export2 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
  -- 0 <- 3
  let sf0Import3 =
        SimpleFunc
          [ SimpleFuncBranch
              SimplePermit
              []
              [SetSimpleNextHop 3, AddSimpleWeight 10]
          ]
  -- 0 <- 1
  let sf0Import1 =
        SimpleFunc
          [ SimpleFuncBranch SimpleDeny [MatchSimpleNextHop 0] []
          , SimpleFuncBranch SimplePermit [] [SetSimpleNextHop 1]
          ]
  -- 1 <- 0
  let sf1Import0 =
        SimpleFunc
          [ SimpleFuncBranch SimpleDeny [MatchSimpleNextHop 1] []
          , SimpleFuncBranch SimplePermit [] [SetSimpleNextHop 0]
          ]
  -- 1 <- 2
  let sf1Import2 =
        SimpleFunc
          [ SimpleFuncBranch SimpleDeny [MatchSimpleNextHop 1] []
          , SimpleFuncBranch SimplePermit [] [SetSimpleNextHop 2]
          ]
  -- 2 <- 1
  let sf2Import1 =
        SimpleFunc
          [ SimpleFuncBranch SimpleDeny [MatchSimpleNextHop 2] []
          , SimpleFuncBranch SimplePermit [] [SetSimpleNextHop 1]
          ]
  -- 2 <- 4
  let sf2Import4 =
        SimpleFunc
          [ SimpleFuncBranch
              SimplePermit
              []
              [SetSimpleNextHop 4, SetSimpleWeight 50]
          ]
  -- print simple functions
  let sfImport0From3 = (Session 0 Import 3, sf0Import3)
  let sfImport1From0 = (Session 1 Import 0, sf1Import0)
  let sfImport0From1 = (Session 0 Import 1, sf0Import1)
  let sfImport2From1 = (Session 2 Import 1, sf2Import1)
  let sfImport1From2 = (Session 1 Import 2, sf1Import2)
  let sfImport2From4 = (Session 2 Import 4, sf2Import4)
  let sfExport3To0 = (Session 3 Export 0, sf3Export0)
  let sfExport0To1 = (Session 0 Export 1, sf0Export1)
  let sfExport1To0 = (Session 1 Export 0, sf1Export0)
  let sfExport1To2 = (Session 1 Export 2, sf1Export2)
  let sfExport2To1 = (Session 2 Export 1, sf2Export1)
  let sfExport4To2 = (Session 4 Export 2, sf4Export2)
  let sfPair03 = (sfExport3To0, sfImport0From3)
  let sfPair01 = (sfExport1To0, sfImport0From1)
  let sfPair10 = (sfExport0To1, sfImport1From0)
  let sfPair12 = (sfExport2To1, sfImport1From2)
  let sfPair21 = (sfExport1To2, sfImport2From1)
  let sfPair24 = (sfExport4To2, sfImport2From4)
  let lPTf03 = toLinkProtoTf intRouters sfPair03
  let lPTf01 = toLinkProtoTf intRouters sfPair01
  let lPTf10 = toLinkProtoTf intRouters sfPair10
  let lPTf12 = toLinkProtoTf intRouters sfPair12
  let lPTf21 = toLinkProtoTf intRouters sfPair21
  let lPTf24 = toLinkProtoTf intRouters sfPair24
  print lPTf03
  print lPTf01
  print lPTf10
  print lPTf12
  print lPTf21
  print lPTf24
  let rTf0 = toRouterProtoTf [lPTf03, lPTf01]
  let rTf1 = toRouterProtoTf [lPTf12, lPTf10]
  let rTf2 = toRouterProtoTf [lPTf21, lPTf24]
  print rTf0
  print rTf1
  print rTf2
  let netTf = toNetProtoTf [rTf0, rTf1, rTf2]
  print netTf
  -- let myAssign = TfAssign
  --       [ TfAssignItem (TfVar "x") (TfVar "y")
  --       , TfAssignItem (TfVar "z") (TfVar "x")
  --       , TfAssignItem (TfVar "y") (TfAdd (TfVar "a1") (TfVar "10"))
  --       ]
  -- print myAssign
  -- print $ unwrapAssign myAssign
  let specs =
        SItem (RouterState (AttrSpec SimpleWeight (Router 1) TfEq (Const "50")))
  let assumps = STrue
  putStrLn $ "Spec: \n" ++ show specs
  let specCond = toSpecCond netTf assumps specs
  writeListToFile condOutPath specCond
  -- specCond <- toSpecCond netTf specs
  -- putStrLn $ "SpecCond: \n" ++ unlines (map show specCond)

mainBgp :: FilePath -> IO ()
mainBgp condOutPath = do
  putStrLn ""
  -- FIXME: this is an ugly API!
  -- maybe use a global state?
  let intRouters = [1, 2]
  let ip1 = ipPrefix "0.0.0.16/28"
  let ip2 = ipPrefix "0.0.0.32/28"
  let pl1 = [ip1, ip2]
  let ip3 = ip "0.0.0.17"
  let ip6 = ip "0.0.0.65"
  let ip4 = ipPrefix "0.0.0.64/28"
  let ip5 = ipPrefix "0.0.0.128/28"
  let pl2 = [ip4, ip5]
  let cl1 = [2001]
  let cl2 = [1001]
  -- (1, 2)
  let rmItem1 = toRmItem BgpPermit [MatchBgpIpPrefix pl1] [SetCommunity 1001]
  let rmItem2 = toRmItem BgpPermit [MatchCommunity cl1] [SetLocalPref 200]
  let rm1Export2 = toBgpRm [rmItem1, rmItem2]
  let rmItem7 = toRmItem BgpPermit [MatchCommunity cl2] [SetLocalPref 100]
  let rmItem8 = toRmItem BgpPermit [] [SetBgpNextHop ip6]
  let rm2Import1 = toBgpRm [rmItem7, rmItem8]
  -- (2, 1)
  let rmItem5 = toRmItem BgpPermit [MatchBgpIpPrefix pl2] [SetLocalPref 50]
  let rmItem6 = toRmItem BgpPermit [] []
  let rm2Export1 = toBgpRm [rmItem5, rmItem6]
  let rmItem3 = toRmItem BgpDeny [MatchCommunity cl2] []
  let rmItem4 = toRmItem BgpPermit [] [SetBgpNextHop ip3]
  let rm1Import2 = toBgpRm [rmItem3, rmItem4]
  -- (3, 1)
  let rmItem9 = toRmItem BgpDeny [MatchCommunity [2001, 1001]] []
  let rmItem10 = toRmItem BgpPermit [] [SetLocalPref 250]
  let rm1Import3 = toBgpRm [rmItem9, rmItem10]
  -- TODO: any deny and permit needs to be declared explicitly
  let rmItem11 = toRmItem BgpPermit [] []
  let rm3Export1 = toBgpRm [rmItem11]
  -- (3, 2)
  let rmItem12 = toRmItem BgpDeny [MatchBgpIpPrefix [ip4]] []
  let rmItem13 = toRmItem BgpPermit [] [SetLocalPref 50]
  let rm2Import3 = toBgpRm [rmItem12, rmItem13]
  let rmItem14 = toRmItem BgpPermit [] []
  let rm3Export2 = toBgpRm [rmItem14]
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
  let sfExport1To2 = (Session 1 Export 2, rm1Export2)
  let sfImport2From1 = (Session 2 Import 1, rm2Import1)
  let sfExport2To1 = (Session 2 Export 1, rm2Export1)
  let sfImport1From2 = (Session 1 Import 2, rm1Import2)
  let sfExport3To1 = (Session 3 Export 1, rm3Export1)
  let sfImport1From3 = (Session 1 Import 3, rm1Import3)
  let sfExport3To2 = (Session 3 Export 2, rm3Export2)
  let sfImport2From3 = (Session 2 Import 3, rm2Import3)
  -- 12 means 1<-2
  let sfPair12 = (sfExport2To1, sfImport1From2)
  let sfPair21 = (sfExport1To2, sfImport2From1)
  let sfPair13 = (sfExport3To1, sfImport1From3)
  let sfPair23 = (sfExport3To2, sfImport2From3)
  let lPTf12 = toLinkProtoTf intRouters sfPair12
  let lPTf21 = toLinkProtoTf intRouters sfPair21
  let lPTf13 = toLinkProtoTf intRouters sfPair13
  let lPTf23 = toLinkProtoTf intRouters sfPair23
  -- -- print session tfs
  -- print (toSimpleSsProtoTf sfExport2To1)
  -- print (toSimpleSsProtoTf intRouters sfImport1From2)
  -- print (toSimpleSsProtoTf sfExport1To2)
  -- print (toSimpleSsProtoTf intRouters sfImport2From1)
  -- print (toSimpleSsProtoTf sfExport3To1)
  -- print (toSimpleSsProtoTf intRouters sfImport1From3)
  -- print (toSimpleSsProtoTf sfExport3To2)
  -- print (toSimpleSsProtoTf intRouters sfImport2From3)
  -- -- print link tfs
  -- print lPTf21
  -- print lPTf12
  -- print lPTf13
  -- print lPTf23
  -- print router tfs
  let rTf1 = toRouterProtoTf [lPTf12, lPTf13]
  let rTf2 = toRouterProtoTf [lPTf21, lPTf23]
  -- print rTf1
  -- print rTf2
  -- print network tfs
  let netTf = toNetProtoTf [rTf1, rTf2]
  -- print netTf
  -- let fpCond = toNetFpCond [rTf1, rTf2]
  -- putStrLn $ "net fp cond:\n" ++ showConds fpCond
  let specs =
        SpecAnd
          (SItem
             (RouterState
                (toAttrSpec BgpIpPrefix (Router 1) TfEq (Const "0.0.0.16/28"))))
          (SItem
             (RouterState
                (toAttrSpec BgpIpPrefix (Router 2) TfEq (Const "0.0.0.16/28"))))
  let assumps = STrue
  -- let assumps =
  --       SItem
  --         (RouterState
  --            (toAttrSpec BgpIpPrefix (Router 3) TfEq (Const "0.0.0.16/28")))
  putStrLn $ "Assump: \n" ++ show assumps ++ "\n"
  putStrLn $ "Spec: \n" ++ show specs
  let specCond = toSpecCond netTf assumps specs
  writeListToFile condOutPath specCond
  -- specCond <- toSpecCond netTf specs

main :: IO ()
main = do
  [condOutPath] <- getArgs
  mainBgp condOutPath
