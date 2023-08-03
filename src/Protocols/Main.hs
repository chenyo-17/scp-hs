module Main where

import           Functions.Transfer
import           Protocols.BGP
import           Protocols.Protocol
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
  let rmItem1 = RmItem Permit [MatchIpPrefix pl1] [SetCommunity 1001]
  let rmItem2 = RmItem Permit [MatchCommunity cl1] [SetLocalPref 200]
  let rmExport1To2 = BgpRm [rmItem1, rmItem2]
  let rmItem7 = RmItem Permit [MatchCommunity cl2] [SetLocalPref 100]
  let rmItem8 = RmItem Permit [] [SetBgpNextHop ip6]
  let rmImport2From1 = BgpRm [rmItem7, rmItem8]
  -- (2, 1)
  let rmItem5 = RmItem Permit [MatchIpPrefix pl2] [SetCommunity 2001]
  let rmItem6 = RmItem Permit [MatchCommunity cl2] [SetLocalPref 200]
  let rmExport2To1 = BgpRm [rmItem5, rmItem6]
  let rmItem3 = RmItem Permit [MatchCommunity cl1] [SetLocalPref 100]
  let rmItem4 = RmItem Permit [] [SetBgpNextHop ip3]
  let rmImport1From2 = BgpRm [rmItem3, rmItem4]
  -- print BGP rms
  putStrLn "BGP route maps:"
  putStrLn $ "R1->R2:\n" ++ show rmExport1To2
  putStrLn $ "R2<-R1:\n" ++ show rmImport2From1
  putStrLn $ "R2->R1:\n" ++ show rmExport2To1
  putStrLn $ "R1<-R2:\n" ++ show rmImport1From2
  let sExport1To2 = Session 1 Export 2
  let sImport2From1 = Session 2 Import 1
  let sExport2To1 = Session 2 Export 1
  let sImport1From2 = Session 1 Import 2
  let rms =
        [ (sExport1To2, rmExport1To2)
        , (sImport2From1, rmImport2From1)
        , (sExport2To1, rmExport2To1)
        , (sImport1From2, rmImport1From2)
        ]
  -- print session tfs
  let sessionTfs = addSessionTfs rms
  putStrLn $ "sessionTfs:\n" ++ showSessionTfs sessionTfs
  -- print link Tfs
  let linkTfs = addLinkTfs rms
  putStrLn $ "linkTfs:\n" ++ showLinkTfs linkTfs
