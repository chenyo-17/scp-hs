module Main
  ( main
  ) where

import           Configurations.Router
import           Control.Exception
import           Control.Parallel.Strategies
import           Data.List                   (foldl')
import           Data.Time.Clock
-- import           Functions.Solver            (simplifyCondWithSolver)
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

timePureFunction :: a -> IO (a, NominalDiffTime)
timePureFunction f = do
  start <- getCurrentTime
  result <- evaluate f
  end <- getCurrentTime
  let diff = diffUTCTime end start
  return (result, diff)

evalAbilene :: FilePath -> IO ()
evalAbilene condOutPath = do
  let intRs = [1, 2, 5, 10, 11]
  let extRs = [12 .. 15] -- 2 providers, 2 customers
  -- 1 rr （5）
  -- all routers are connected to rr
  let conns =
        [ (1, 5)
        , (2, 5)
        , (5, 11)
        , (5, 10)
        , (1, 12)
        , (11, 13)
        , (2, 14)
        , (10, 15)
        ]
  let cust1_pl = [ipPrefix "0.0.0.12/28"]
  let cust2_pl = [ipPrefix "0.0.0.13/28"]
  -- cust1 -> 1
  let rm1ImpCust1 =
        toBgpRm
          [ toRmItem
              BgpPermit
              [MatchBgpIpPrefix cust1_pl]
              [SetLocalPref 1000, SetCommunity 12]
          ]
  -- cust2 -> 11
  let rm11ImpCust2 =
        toBgpRm
          [ toRmItem
              BgpPermit
              [MatchBgpIpPrefix cust2_pl]
              [SetLocalPref 1000, SetCommunity 13]
          ]
  -- prov1 --> 2
  let rm2ImpProv1 =
        toBgpRm [toRmItem BgpPermit [] [SetLocalPref 100, SetCommunity 14]]
  -- prov2 --> 10
  let rm10ImpProv2 =
        toBgpRm [toRmItem BgpPermit [] [SetLocalPref 100, SetCommunity 15]]
  -- 1 --> cust1_o
  let rm1ExpCust1o =
        toBgpRm [toRmItem BgpPermit [MatchCommunity [13, 14, 15]] []]
  -- 11 --> cust2_o
  let rm11ExpCust2o =
        toBgpRm [toRmItem BgpPermit [MatchCommunity [12, 14, 15]] []]
  -- 2 --> prov1_o
  let rm2ExpProv1o = toBgpRm [toRmItem BgpPermit [MatchCommunity [12, 13]] []]
  -- 10 --> prov2_o
  let rm10ExpProv2o = toBgpRm [toRmItem BgpPermit [MatchCommunity [12, 13]] []]
  -- print BGP rms
  putStrLn $ "BGPrm R1<-cust1:\n" ++ show rm1ImpCust1
  putStrLn $ "BGPrm R11<-cust2:\n" ++ show rm11ImpCust2
  putStrLn $ "BGPrm R2<-prov1:\n" ++ show rm2ImpProv1
  putStrLn $ "BGPrm R10<-prov2:\n" ++ show rm10ImpProv2
  putStrLn $ "BGPrm R1->cust1:\n" ++ show rm1ExpCust1o
  putStrLn $ "BGPrm R11->cust2:\n" ++ show rm11ExpCust2o
  putStrLn $ "BGPrm R2->prov1:\n" ++ show rm2ExpProv1o
  putStrLn $ "BGPrm R10->prov2:\n" ++ show rm10ExpProv2o
  let allConfigs =
        [ (Session 1 Import 12, rm1ImpCust1)
        , (Session 11 Import 13, rm11ImpCust2)
        , (Session 2 Import 14, rm2ImpProv1)
        , (Session 10 Import 15, rm10ImpProv2)
        , (Session 1 Export 12, rm1ExpCust1o)
        , (Session 11 Export 13, rm11ExpCust2o)
        , (Session 2 Export 14, rm2ExpProv1o)
        , (Session 10 Export 15, rm10ExpProv2o)
        ]
  let ssTfs = foldl' (\acc (s, a) -> addRouterConfig intRs acc s a) [] allConfigs
--   putStrLn $ "Time to add router configs: " ++ show elapsed
      --   foldl' (\acc (s, a) -> addRouterConfig intRs acc s a) [] allConfigs
--   putStrLn $ unlines (map show ssTfs)
  -- use concurrency
  let rTfs = parMap
         rseq
         (\rId -> toRouterTf rId intRs (getNbs conns rId) ssTfs)
         (intRs ++ extRs)
--   putStrLn $ unlines (map show rTfs)
--   print rTfs
--   putStrLn $ "Time to add router tfs: " ++ show elapsed
  let netTf = toNetProtoTf rTfs
--   print netTf
--   print netTf
--   putStrLn $ unlines (map show rTfs)
--   (netTf, elapsed) <-
--     timePureFunction (toNetProtoTf rTfs)
--   putStrLn $ "Time to add network tfs: " ++ show elapsed
  let assump1 =
        SItem
          (RouterState (toAttrSpec BgpIpPrefix (Router 15) TfEq (Symbol "p")))
  let assump2 =
        SItem
          (RouterState (toAttrSpec BgpIpPrefix (Router 14) TfEq (Symbol "p")))
  let assump3 =
        SItem
          (RouterState
             (toAttrSpec BgpIpPrefix (Symbol "p") TfNe (Symbol "null")))
  let assumps = SpecAnd assump3 (SpecOr assump1 assump2)
  putStrLn $ "Assump: \n" ++ show assumps ++ "\n"
  let specs =
        SItem
          (RouterState
             (toAttrSpec BgpNextHop (Router (getExtId 12)) TfEq (Symbol "p")))
  putStrLn $ "Spec: \n" ++ show specs
  let specCond = toSpecCond netTf assumps specs
-- --   (specCond, elapsed) <-
-- --     timePureFunction (toSpecCond netTf assumps specs)
-- --   putStrLn $ "Time to add spec conditions: " ++ show elapsed
  writeListToFile condOutPath specCond
  -- let assump1 =
  --       SItem
  --         (RouterState (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "p")))
  -- let assump2 =
  --       SItem
  --         (RouterState (toAttrSpec BgpIpPrefix (Router 6) TfEq (Symbol "p")))
  -- let assump3 =
  --       SItem
  --         (RouterState
  --            (toAttrSpec BgpIpPrefix (Symbol "p") TfNe (Symbol "null")))
  -- let assumps = SpecAnd assump3 (SpecOr assump1 assump2)
  --           --  (SItem
  --           --     (RouterState
  --           --        (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "p")))))
  --           --  (SpecOr
  --           --     (SItem
  --           --        (RouterState
  --           --           (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "p"))))
  --           --     (SItem
  --           --        (RouterState
  --           --           (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "null"))))))
  -- putStrLn $ "Assump: \n" ++ show assumps ++ "\n"

--   let ssTfs =
--   putStrLn $ unlines (map show specCond)
mainBgp2 :: FilePath -> IO ()
mainBgp2 condOutPath = do
  -- 0 -- internal
  -- 1 -- cust(4), prov1(5)
  -- 2 -- prov2(6)
  -- 3 -- RR
  let intRs = [0, 1, 2, 3] -- used to decide whether to add deny BgpFrom
  let extRs = [4, 5, 6]
  let conns = [(1, 3), (1, 4), (1, 5), (2, 3), (2, 6), (3, 0)]
  let cust_pl = [ipPrefix "0.0.0.16/28"]
  -- cust -> 1
  let rm1ImpCust =
        toBgpRm
          [ toRmItem
              BgpPermit
              [MatchBgpIpPrefix cust_pl]
              [SetLocalPref 1000, SetCommunity 0]
          ]
            -- SetCommunity 0 means delete all community
  -- prov1 --> 1
  let rm1ImpProv1 =
        toBgpRm [toRmItem BgpPermit [] [SetLocalPref 100, SetCommunity 0]]
  -- prov2 --> 2
  let rm2ImpProv2 =
        toBgpRm [toRmItem BgpPermit [] [SetLocalPref 150, SetCommunity 22]]
  -- 1 --> cust_o
  let rm1ExpCusto = toBgpRm [toRmItem BgpPermit [MatchCommunity [22]] []]
  -- cust_o --> 1
  -- 1 --> 3
  -- print BGP rms
  putStrLn $ "BGPrm R1<-cust:\n" ++ show rm1ImpCust
  putStrLn $ "BGPrm R1<-prov1:\n" ++ show rm1ImpProv1
  putStrLn $ "BGPrm R2<-prov2:\n" ++ show rm2ImpProv2
  putStrLn $ "BGPrm cust<-R1:\n" ++ show rm1ExpCusto
  -- add router tf
  let allConfigs =
        [ (Session 1 Import 4, rm1ImpCust)
        , (Session 1 Import 5, rm1ImpProv1)
        , (Session 2 Import 6, rm2ImpProv2)
        , (Session 1 Export 4, rm1ExpCusto)
        ]
  let ssTfs =
        foldl' (\acc (s, a) -> addRouterConfig intRs acc s a) [] allConfigs
  let rTfs =
        map
          (\rId -> toRouterTf rId intRs (getNbs conns rId) ssTfs)
          (intRs ++ extRs)
  let netTf = toNetProtoTf rTfs
  -- print one session tf per line
  -- map each router to its router tf
  -- get the network tf
  -- get the last clause of netTf
  let assump1 =
        SItem
          (RouterState (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "p")))
  let assump2 =
        SItem
          (RouterState (toAttrSpec BgpIpPrefix (Router 6) TfEq (Symbol "p")))
  let assump3 =
        SItem
          (RouterState
             (toAttrSpec BgpIpPrefix (Symbol "p") TfNe (Symbol "null")))
  let assumps = SpecAnd assump3 (SpecOr assump1 assump2)
            --  (SItem
            --     (RouterState
            --        (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "p")))))
            --  (SpecOr
            --     (SItem
            --        (RouterState
            --           (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "p"))))
            --     (SItem
            --        (RouterState
            --           (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "null"))))))
  putStrLn $ "Assump: \n" ++ show assumps ++ "\n"
  -- spec: all routes imported from prov1 or prov2 must be exported to cust
  -- all external neighbors announce the same prefix
  -- let specs =
  --       SpecOr
  --         (SItem
  --            (RouterState (toAttrSpec BgpOrigin (Router 7) TfEq (Const "4"))))
  --         (SpecAnd
  --            (SItem
  --               (RouterState (toAttrSpec BgpOrigin (Router 7) TfEq (Const "5"))))
  --            (SItem
                -- (RouterState (toAttrSpec BgpOrigin (Router 7) TfEq (Const "6")))))
  let specs =
        SItem
          (RouterState
             (toAttrSpec BgpNextHop (Router (getExtId 4)) TfEq (Symbol "p")))
  putStrLn $ "Spec: \n" ++ show specs
  let specCond = toSpecCond netTf assumps specs
  writeListToFile condOutPath specCond

--   let tmp = addRouterConfig intRs [] (Session 1 Export 4) rm1ExpCusto
--   print tmp
--   putStrLn $ unlines (map show ssTfs)
--   print (getSimpleDefaultSsProtoTf intRs (Session 3 Export 0) (head ssTfs))
--   print (getSimpleDefaultSsProtoTf intRs (Session 0 Import 3) (head ssTfs))
--   print (findSsTf (Session 3 Export 0) ssTfs)
--   print (findSsTf (Session 0 Import 3) ssTfs)
--   let tmp = toRouterTf 1 intRs (getNbs conns 1) ssTfs
--   print tmp
--   print $ getSimpleDefaultSsProtoTf intRs (Session 1 Export 5) (head ssTfs)
--   print (findSsTf intRs (Session 1 Import 4) ssTfs)
--   putStrLn $ unlines (map show rTfs)
--   print netTf
--   let toRTf rId = toRouterTf rId intRs (getNbs conns rId) ssTfs
--   let rTfs = map toRTf (intRs ++ extRs)
--   putStrLn $ "Router tfs: \n" ++ unlines (map show rTfs)
--   print rTfs
--   let netTf = toNetProtoTf rTfs
--   print netTf
--   assumps: each external router has a unique BgpOrigin
--   putStrLn $ unlines (map show specCond)
--   -- link tfs
--   -- 1 <-- cust
--   let link1Cust =
--         ((Session 4 Export 1, rmCustExp1), (Session 1 Import 4, rm1ImpCust))
--   let lTf1Cust = toLinkProtoTf intRs link1Cust
--   -- 1 <-- prov1
--   let link1Prov1 =
--         ((Session 5 Export 1, rmProv1Exp1), (Session 1 Import 5, rm1ImpProv1))
--   let lTf1Prov1 = toLinkProtoTf intRs link1Prov1
--   -- 2 <-- prov2
--   let link2Prov2 =
--         ((Session 6 Export 2, rmProv2Exp2), (Session 2 Import 6, rm2ImpProv2))
--   let lTf2Prov2 = toLinkProtoTf intRs link2Prov2
--   print lTf2Prov2
--   -- 1 <-- RR
--   let link13 = ((Session 3 Export 1, rm3Exp1), (Session 1 Import 3, rm1Imp3))
--   let lTf13 = toLinkProtoTf intRs link13
--   -- 2 <-- RR
--   let link23 = ((Session 3 Export 2, rm3Exp2), (Session 2 Import 3, rm2Imp3))
--   let lTf23 = toLinkProtoTf intRs link23
--   print lTf23
--   -- 0 <-- RR
--   let link03 = ((Session 3 Export 0, rm3Exp0), (Session 0 Import 3, rm0Imp3))
--   let lTf03 = toLinkProtoTf intRs link03
--   print lTf03
--   -- RR <-- 1
--   let link31 = ((Session 1 Export 3, rm1Exp3), (Session 3 Import 1, rm3Imp1))
--   let lTf31 = toLinkProtoTf intRs link31
--   -- RR <-- 2
--   let link32 = ((Session 2 Export 3, rm2Exp3), (Session 3 Import 2, rm3Imp2))
--   let lTf32 = toLinkProtoTf intRs link32
--   -- cust_o <-- 1
--   let linkCusto1 =
--         ((Session 1 Export 7, rm1ExpCusto), (Session 7 Import 1, rmCustoImp1))
--   let lTfCusto1 = toLinkProtoTf intRs linkCusto1
--   -- router tfs
--   let rTf1 = toRouterProtoTf [lTf1Cust, lTf1Prov1, lTf13]
--   print rTf1
--   let rTf2 = toRouterProtoTf [lTf2Prov2, lTf23]
--   print rTf2
--   let rTf3 = toRouterProtoTf [lTf31, lTf32]
--   print rTf3
--   let rTfCusto = toRouterProtoTf [lTfCusto1]
--   let rTf0 = toRouterProtoTf [lTf03]
--   -- network tfs
--   let netTf = toNetProtoTf [rTf3, rTf1, rTf2, rTfCusto, rTf0]
--   print netTf
--   let assumps =
--         SpecAnd
--           (SItem
--              (RouterState
--                 (toAttrSpec BgpIpPrefix (Symbol "p") TfNe (Symbol "null"))))
--       --     (SpecOr
--              (SItem
--                 (RouterState
--                    (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "p"))))
--   -- 0 --> 3
--   let rm0Exp3 = permitAll
--   let rm3Imp0 = permitAll
--   print rTf1
--   print rTfCusto
--   let assumps = STrue
--   let assumps = SpecOr (SItem (RouterState (toAttrSpec BgpIpPrefix (Router 6) TfEq (Symbol "null"))))
--                        (SItem (RouterState (toAttrSpec BgpIpPrefix (Router 6) TfEq (Symbol "p"))))
--   let assumps =
--         SpecAnd
--           (SItem
--              (RouterState
--                 (toAttrSpec BgpIpPrefix (Symbol "p") TfNe (Symbol "null"))))
--           $ SpecAnd
--               (SItem
--                  (RouterState
--                     (toAttrSpec BgpIpPrefix (Router 5) TfEq (Symbol "p"))))
--               (SpecAnd
--                  (SpecOr
--                     (SItem
--                        (RouterState
--                           (toAttrSpec BgpIpPrefix (Router 4) TfEq (Symbol "p"))))
--                     (SItem
--                        (RouterState
--                           (toAttrSpec
--                              BgpIpPrefix
--                              (Router 4)
--                              TfEq
--                              (Symbol "null")))))
--                  (SpecOr
--                     (SItem
--                        (RouterState
--                           (toAttrSpec BgpIpPrefix (Router 6) TfEq (Symbol "p"))))
--                     (SItem
--                        (RouterState
--                           (toAttrSpec
--                              BgpIpPrefix
--                              (Router 6)
--                              TfEq
--                              (Symbol "null"))))))
--   print lTf1Cust
--   print lTf1Prov1
--   print lTf13
--   let assumps = SpecAnd (SItem (RouterState (toAttrSpec BgpIpPrefix (Router 1) TfEq (Router 5))))
--         (SpecAnd
--           (SItem
--              (RouterState (toAttrSpec BgpIpPrefix (Router 1) TfEq (Router 2))))
--           (SItem
--              (RouterState (toAttrSpec BgpIpPrefix (Router 2) TfEq (Router 3)))))
--   putStrLn $ "Assump: \n" ++ show assumps ++ "\n"
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
      -- mainBgp2 "test.txt"
  [condOutPath] <- getArgs
--   mainBgp2 condOutPath
  evalAbilene condOutPath
