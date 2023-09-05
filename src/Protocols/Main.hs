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

-- mainSimple :: FilePath -> Bool -> IO ()
-- mainSimple condOutPath getVio = do
--   putStrLn ""
--   -- all export funcs are empty
--   -- TODO: automate this
--   let intRouters = [0, 1, 2]
--   let sf3Export0 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
--   let sf0Export1 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
--   let sf1Export0 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
--   let sf1Export2 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
--   let sf2Export1 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
--   let sf4Export2 = SimpleFunc [SimpleFuncBranch SimplePermit [] []]
--   -- 0 <- 3
--   let sf0Import3 =
--         SimpleFunc
--           [ SimpleFuncBranch
--               SimplePermit
--               []
--               [SetSimpleNextHop 3, AddSimpleWeight 10]
--           ]
--   -- 0 <- 1
--   let sf0Import1 =
--         SimpleFunc
--           [ SimpleFuncBranch SimpleDeny [MatchSimpleNextHop 0] []
--           , SimpleFuncBranch SimplePermit [] [SetSimpleNextHop 1]
--           ]
--   -- 1 <- 0
--   let sf1Import0 =
--         SimpleFunc
--           [ SimpleFuncBranch SimpleDeny [MatchSimpleNextHop 1] []
--           , SimpleFuncBranch SimplePermit [] [SetSimpleNextHop 0]
--           ]
--   -- 1 <- 2
--   let sf1Import2 =
--         SimpleFunc
--           [ SimpleFuncBranch SimpleDeny [MatchSimpleNextHop 1] []
--           , SimpleFuncBranch SimplePermit [] [SetSimpleNextHop 2]
--           ]
--   -- 2 <- 1
--   let sf2Import1 =
--         SimpleFunc
--           [ SimpleFuncBranch SimpleDeny [MatchSimpleNextHop 2] []
--           , SimpleFuncBranch SimplePermit [] [SetSimpleNextHop 1]
--           ]
--   -- 2 <- 4
--   let sf2Import4 =
--         SimpleFunc
--           [ SimpleFuncBranch
--               SimplePermit
--               []
--               [SetSimpleNextHop 4, SetSimpleWeight 50]
--           ]
--   -- print simple functions
--   let sfImport0From3 = (Session 0 Import 3, sf0Import3)
--   let sfImport1From0 = (Session 1 Import 0, sf1Import0)
--   let sfImport0From1 = (Session 0 Import 1, sf0Import1)
--   let sfImport2From1 = (Session 2 Import 1, sf2Import1)
--   let sfImport1From2 = (Session 1 Import 2, sf1Import2)
--   let sfImport2From4 = (Session 2 Import 4, sf2Import4)
--   let sfExport3To0 = (Session 3 Export 0, sf3Export0)
--   let sfExport0To1 = (Session 0 Export 1, sf0Export1)
--   let sfExport1To0 = (Session 1 Export 0, sf1Export0)
--   let sfExport1To2 = (Session 1 Export 2, sf1Export2)
--   let sfExport2To1 = (Session 2 Export 1, sf2Export1)
--   let sfExport4To2 = (Session 4 Export 2, sf4Export2)
--   let sfPair03 = (sfExport3To0, sfImport0From3)
--   let sfPair01 = (sfExport1To0, sfImport0From1)
--   let sfPair10 = (sfExport0To1, sfImport1From0)
--   let sfPair12 = (sfExport2To1, sfImport1From2)
--   let sfPair21 = (sfExport1To2, sfImport2From1)
--   let sfPair24 = (sfExport4To2, sfImport2From4)
--   let lPTf03 = toLinkProtoTf intRouters sfPair03
--   let lPTf01 = toLinkProtoTf intRouters sfPair01
--   let lPTf10 = toLinkProtoTf intRouters sfPair10
--   let lPTf12 = toLinkProtoTf intRouters sfPair12
--   let lPTf21 = toLinkProtoTf intRouters sfPair21
--   let lPTf24 = toLinkProtoTf intRouters sfPair24
--   print lPTf03
--   print lPTf01
--   print lPTf10
--   print lPTf12
--   print lPTf21
--   print lPTf24
--   let rTf0 = toRouterProtoTf [lPTf03, lPTf01]
--   let rTf1 = toRouterProtoTf [lPTf12, lPTf10]
--   let rTf2 = toRouterProtoTf [lPTf21, lPTf24]
--   print rTf0
--   print rTf1
--   print rTf2
--   let netTf = toNetProtoTf [rTf0, rTf1, rTf2]
--   -- let myAssign = TfAssign
--   --       [ TfAssignItem (TfVar "x") (TfVar "y")
--   --       , TfAssignItem (TfVar "z") (TfVar "x")
--   --       , TfAssignItem (TfVar "y") (TfAdd (TfVar "a1") (TfVar "10"))
--   --       ]
--   -- print myAssign
--   -- print $ unwrapAssign myAssign
--   let specs =
--         SItem (RouterState (AttrSpec SimpleWeight (Router 1) TfEq (Const "50")))
--   let assumps = STrue
--   putStrLn $ "Spec: \n" ++ show specs
--   let specCond = toSpecCond netTf assumps specs getVio
--   writeListToFile condOutPath specCond
--   -- specCond <- toSpecCond netTf specs
--   -- putStrLn $ "SpecCond: \n" ++ unlines (map show specCond)

--   print netTf
mainBgp2 :: FilePath -> Bool -> IO ()
mainBgp2 condOutPath getVio = do
  -- 1 -- cust(4), prov1(5)
  -- 2 -- prov2(6)
  -- 3 -- RR
  -- 7 -- cust_o (to check what does R1 export to cust)
  let intRouters = [1, 2, 3] -- used to decide whether to add deny BgpFrom
  let cust_pl = [ipPrefix "0.0.0.16/28"]
  let permitAll = toBgpRm [toRmItem BgpPermit [] []]
  -- cust -> 1
  let rmCustExp1 = permitAll
  let rm1ImpCust =
        toBgpRm
          [ toRmItem
              BgpPermit
              [MatchBgpIpPrefix cust_pl]
              [SetLocalPref 1000, SetCommunity 0]
          ]
            -- SetCommunity 0 means delete all community
  -- prov1 --> 1
  let rmProv1Exp1 = permitAll
  let rm1ImpProv1 =
        toBgpRm [toRmItem BgpPermit [] [SetLocalPref 100, SetCommunity 0]]
  -- prov2 --> 2
  let rmProv2Exp2 = permitAll
  let rm2ImpProv2 =
        toBgpRm [toRmItem BgpPermit [] [SetLocalPref 150, SetCommunity 0]]
  -- 1 --> cust_o
  let rm1ExpCusto = toBgpRm [toRmItem BgpPermit [MatchCommunity [22]] []]
  let rmCustoImp1 = permitAll
  -- cust_o --> 1
  let rmCustoExp1 = toBgpRm [toRmItem BgpDeny [] []]
  let rm1ImpCusto = toBgpRm [toRmItem BgpDeny [] []]
  -- 1 --> 3
  let rm1Exp3 = permitAll
  let rm3Imp1 = permitAll
  -- 2 --> 3
  let rm2Exp3 = permitAll
  let rm3Imp2 = toBgpRm [toRmItem BgpPermit [] [SetCommunity 22]]
  -- 3 --> 1
  let rm3Exp1 = permitAll
  let rm1Imp3 = permitAll
  -- 3 --> 2
  let rm3Exp2 = permitAll
  let rm2Imp3 = permitAll
  -- print BGP rms
  putStrLn $ "BGPrm R1<-cust:\n" ++ show rm1ImpCust
  putStrLn $ "BGPrm R1<-prov1:\n" ++ show rm1ImpProv1
  putStrLn $ "BGPrm R2<-prov2:\n" ++ show rm2ImpProv2
  putStrLn $ "BGPrm RR<-R2:\n" ++ show rm3Imp2
  putStrLn $ "BGPrm cust<-R1:\n" ++ show rm1ExpCusto
  -- link tfs
  -- 1 <-- cust
  let link1Cust =
        ((Session 4 Export 1, rmCustExp1), (Session 1 Import 4, rm1ImpCust))
  let lTf1Cust = toLinkProtoTf intRouters link1Cust
  -- 1 <-- prov1
  let link1Prov1 =
        ((Session 5 Export 1, rmProv1Exp1), (Session 1 Import 5, rm1ImpProv1))
  let lTf1Prov1 = toLinkProtoTf intRouters link1Prov1
  -- 2 <-- prov2
  let link2Prov2 =
        ((Session 6 Export 2, rmProv2Exp2), (Session 2 Import 6, rm2ImpProv2))
  let lTf2Prov2 = toLinkProtoTf intRouters link2Prov2
  -- 1 <-- RR
  let link13 = ((Session 3 Export 1, rm3Exp1), (Session 1 Import 3, rm1Imp3))
  let lTf13 = toLinkProtoTf intRouters link13
  -- 2 <-- RR
  let link23 = ((Session 3 Export 2, rm3Exp2), (Session 2 Import 3, rm2Imp3))
  let lTf23 = toLinkProtoTf intRouters link23
  -- RR <-- 1
  let link31 = ((Session 1 Export 3, rm1Exp3), (Session 3 Import 1, rm3Imp1))
  let lTf31 = toLinkProtoTf intRouters link31
  -- RR <-- 2
  let link32 = ((Session 2 Export 3, rm2Exp3), (Session 3 Import 2, rm3Imp2))
  let lTf32 = toLinkProtoTf intRouters link32
  -- cust_o <-- 1
  let linkCusto1 =
        ((Session 1 Export 7, rm1ExpCusto), (Session 7 Import 1, rmCustoImp1))
  let lTfCusto1 = toLinkProtoTf intRouters linkCusto1
  -- router tfs
  let rTf1 = toRouterProtoTf [lTf1Cust, lTf1Prov1, lTf13]
  print rTf1
  let rTf2 = toRouterProtoTf [lTf2Prov2, lTf23]
  print rTf2
  let rTf3 = toRouterProtoTf [lTf31, lTf32]
  print rTf3
  let rTfCusto = toRouterProtoTf [lTfCusto1]
  print rTfCusto
  -- network tfs
  let netTf = toNetProtoTf [rTf3, rTf1, rTf2, rTfCusto]
  -- print netTf
  -- get the last clause of netTf
  -- assumps: each external router has a unique BgpOrigin
  let assumps =
        SpecAnd
          (SpecAnd
             (SItem
                (RouterState (toAttrSpec BgpOrigin (Router 4) TfEq (Const "4"))))
             (SItem
                (RouterState (toAttrSpec BgpOrigin (Router 5) TfEq (Const "5")))))
          (SItem (RouterState (toAttrSpec BgpOrigin (Router 6) TfEq (Const "6"))))
  -- spec: all routes imported from prov1 or prov2 must be exported to cust
  -- all external neighbors announce the same prefix
  let specs =
        SpecImply
          (SpecAnd
             (SItem
                (RouterState (toAttrSpec BgpIpPrefix (Router 4) TfEq (Router 5))))
             (SItem
                (RouterState (toAttrSpec BgpIpPrefix (Router 5) TfEq (Router 6)))))
          (SpecOr
             (SItem
                (RouterState (toAttrSpec BgpOrigin (Router 7) TfEq (Const "5"))))
             (SItem
                (RouterState (toAttrSpec BgpOrigin (Router 7) TfEq (Const "6")))))
  putStrLn $ "Spec: \n" ++ show specs
  let specCond = toSpecCond netTf assumps specs getVio
  -- putStrLn $ unlines (map show specCond)
  putStrLn
    $ if getVio
        then "\nComputing all violations:"
        else "\nComputing all satisfiability:"
  writeListToFile condOutPath specCond

--   putStrLn $ "Assump: \n" ++ show assumps ++ "\n"
--   print lTf1Cust
--   print lTf1Prov1
--   print lTf2Prov2
--   print lTf23
--   print lTf13
--   print lTf31
--   print lTf32
--   print rTf1
--   print rTf2
--   print rTf3
--   print netTf
--   let assumps =
--         SpecAnd
--           (SItem
--              (RouterState (toAttrSpec BgpIpPrefix (Router 5) TfNe (Router 6))))
--           (SItem
--              (RouterState (toAttrSpec BgpIpPrefix (Router 6) TfEq (Router 7))))
-- mainBgp :: FilePath -> IO ()
-- mainBgp condOutPath = do
--   putStrLn ""
--   -- FIXME: this is an ugly API!
--   -- maybe use a global state?
--   let intRouters = [1, 2]
--   let ip1 = ipPrefix "0.0.0.16/28"
--   let ip2 = ipPrefix "0.0.0.32/28"
--   let pl1 = [ip1, ip2]
--   let ip3 = ip "0.0.0.17"
--   let ip6 = ip "0.0.0.65"
--   let ip4 = ipPrefix "0.0.0.64/28"
--   let ip5 = ipPrefix "0.0.0.128/28"
--   let pl2 = [ip4, ip5]
--   let cl1 = [2001]
--   let cl2 = [1001]
--   -- (1, 2)
--   let rmItem1 = toRmItem BgpPermit [MatchBgpIpPrefix pl1] [SetCommunity 1001]
--   let rmItem2 = toRmItem BgpPermit [MatchCommunity cl1] [SetLocalPref 200]
--   let rm1Export2 = toBgpRm [rmItem1, rmItem2]
--   let rmItem7 = toRmItem BgpPermit [MatchCommunity cl2] [SetLocalPref 100]
--   let rmItem8 = toRmItem BgpPermit [] [SetBgpNextHop ip6]
--   let rm2Import1 = toBgpRm [rmItem7, rmItem8]
--   -- (2, 1)
--   let rmItem5 = toRmItem BgpPermit [MatchBgpIpPrefix pl2] [SetLocalPref 50]
--   let rmItem6 = toRmItem BgpPermit [] []
--   let rm2Export1 = toBgpRm [rmItem5, rmItem6]
--   let rmItem3 = toRmItem BgpDeny [MatchCommunity cl2] []
--   let rmItem4 = toRmItem BgpPermit [] [SetBgpNextHop ip3]
--   let rm1Import2 = toBgpRm [rmItem3, rmItem4]
--   -- (3, 1)
--   let rmItem9 = toRmItem BgpDeny [MatchCommunity [2001, 1001]] []
--   let rmItem10 = toRmItem BgpPermit [] [SetLocalPref 250]
--   let rm1Import3 = toBgpRm [rmItem9, rmItem10]
--   -- TODO: any deny and permit needs to be declared explicitly
--   let rmItem11 = toRmItem BgpPermit [] []
--   let rm3Export1 = toBgpRm [rmItem11]
--   -- (3, 2)
--   let rmItem12 = toRmItem BgpDeny [MatchBgpIpPrefix [ip4]] []
--   let rmItem13 = toRmItem BgpPermit [] [SetLocalPref 50]
--   let rm2Import3 = toBgpRm [rmItem12, rmItem13]
--   let rmItem14 = toRmItem BgpPermit [] []
--   let rm3Export2 = toBgpRm [rmItem14]
--   -- print BGP rms
--   putStrLn $ "BGPrm 1->2:\n" ++ show rm1Export2
--   putStrLn $ "BGPrm 2<-1:\n" ++ show rm2Import1
--   putStrLn $ "BGPrm 2->1:\n" ++ show rm2Export1
--   putStrLn $ "BGPrm 1<-2:\n" ++ show rm1Import2
--   putStrLn $ "BGPrm 3->1:\n" ++ show rm3Export1
--   putStrLn $ "BGPrm 1<-3:\n" ++ show rm1Import3
--   putStrLn $ "BGPrm 3->2:\n" ++ show rm3Export2
--   putStrLn $ "BGPrm 2<-3:\n" ++ show rm2Import3
--   -- FIXME: this is not a good API!
--   let sfExport1To2 = (Session 1 Export 2, rm1Export2)
--   let sfImport2From1 = (Session 2 Import 1, rm2Import1)
--   let sfExport2To1 = (Session 2 Export 1, rm2Export1)
--   let sfImport1From2 = (Session 1 Import 2, rm1Import2)
--   let sfExport3To1 = (Session 3 Export 1, rm3Export1)
--   let sfImport1From3 = (Session 1 Import 3, rm1Import3)
--   let sfExport3To2 = (Session 3 Export 2, rm3Export2)
--   let sfImport2From3 = (Session 2 Import 3, rm2Import3)
--   -- 12 means 1<-2
--   let sfPair12 = (sfExport2To1, sfImport1From2)
--   let sfPair21 = (sfExport1To2, sfImport2From1)
--   let sfPair13 = (sfExport3To1, sfImport1From3)
--   let sfPair23 = (sfExport3To2, sfImport2From3)
--   let lPTf12 = toLinkProtoTf intRouters sfPair12
--   let lPTf21 = toLinkProtoTf intRouters sfPair21
--   let lPTf13 = toLinkProtoTf intRouters sfPair13
--   let lPTf23 = toLinkProtoTf intRouters sfPair23
--   -- -- print session tfs
--   -- print (toSimpleSsProtoTf sfExport2To1)
--   -- print (toSimpleSsProtoTf intRouters sfImport1From2)
--   -- print (toSimpleSsProtoTf sfExport1To2)
--   -- print (toSimpleSsProtoTf intRouters sfImport2From1)
--   -- print (toSimpleSsProtoTf sfExport3To1)
--   -- print (toSimpleSsProtoTf intRouters sfImport1From3)
--   -- print (toSimpleSsProtoTf sfExport3To2)
--   -- print (toSimpleSsProtoTf intRouters sfImport2From3)
--   -- -- print link tfs
--   -- print lPTf21
--   -- print lPTf12
--   -- print lPTf13
--   -- print lPTf23
--   -- print router tfs
--   let rTf1 = toRouterProtoTf [lPTf12, lPTf13]
--   let rTf2 = toRouterProtoTf [lPTf21, lPTf23]
--   -- print rTf1
--   -- print rTf2
--   -- print network tfs
--   let netTf = toNetProtoTf [rTf1, rTf2]
--   -- print netTf
--   -- let fpCond = toNetFpCond [rTf1, rTf2]
--   -- putStrLn $ "net fp cond:\n" ++ showConds fpCond
--   let specs =
--         SpecAnd
--           (SItem
--              (RouterState
--                 (toAttrSpec BgpIpPrefix (Router 1) TfEq (Const "0.0.0.16/28"))))
--           (SItem
--              (RouterState
--                 (toAttrSpec BgpIpPrefix (Router 2) TfEq (Const "0.0.0.16/28"))))
--   let assumps = STrue
--   -- let assumps =
--   --       SItem
--   --         (RouterState
--   --            (toAttrSpec BgpIpPrefix (Router 3) TfEq (Const "0.0.0.16/28")))
--   putStrLn $ "Assump: \n" ++ show assumps ++ "\n"
--   putStrLn $ "Spec: \n" ++ show specs
--   let specCond = toSpecCond netTf assumps specs
--   writeListToFile condOutPath specCond
--   -- specCond <- toSpecCond netTf specs
main :: IO ()
main = do
  [condOutPath] <- getArgs
  mainBgp2 condOutPath True
