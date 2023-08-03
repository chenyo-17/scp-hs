module Main where

import           Functions.Transfer
import           Protocols.BGP
import           Protocols.Protocol
import           Utilities.Ip

main :: IO ()
main = do
  let ip1 = read "17.0.0.0/8" :: IpPrefix
  let ip2 = read "19.0.0.0/8" :: IpPrefix
  let ipDeny = read "10.0.0.0/8" :: IpPrefix
  let pl1 = [ip1, ip2]
  let plDeny = [ipDeny]
  let cl1 = [70, 80]
  let rmItem1 = RmItem Deny [MatchIpPrefix plDeny] []
  let rmItem2 = RmItem Permit [MatchIpPrefix pl1] [SetCommunity 70]
  -- export
  let rm1 = BgpRm [rmItem1, rmItem2]
  let rmItem3 = RmItem Permit [MatchCommunity cl1] [SetLocalPref 100]
  -- import
  let rm2 = BgpRm [rmItem3]
  -- print $ toSimpleTf rm1
  -- print $ toSimpleTf rm2
  -- print $ addSessionTf (Session 2 Import 1) rm1 []
  -- print $ addSessionTf (Session 1 Export 2) rm2 []

  -- collect all session tfs
  -- let sessionTfs = [(Session 2 Import 1, rm2), (Session 1 Export 2, rm1)]
  -- print $ addSessionTfs sessionTfs
  let sessionTf1 = head $ addSessionTf (Session 1 Export 2) rm1 []
  let sessionTf2 = head $ addSessionTf (Session 2 Import 1) rm2 []

  -- putStrLn $ "cPair: " ++ show (prod2Tfs sessionTf1 sessionTf2)
  -- print $ "cPair: " ++ show cPair
  -- print $ "concat: " ++ show (concatTfClause cPair)
  -- putStrLn ""
  putStrLn $ "sessionTf1:\n" ++ (show . ssTf) sessionTf1
  putStrLn $ "sessionTf2:\n" ++ (show . ssTf) sessionTf2

  let linkTf = toLinkTfs [sessionTf1, sessionTf2]
  -- let linkTf2 = toLinkTf sessionTf2 []

  putStrLn $ "linkTf:\n" ++ show linkTf
  -- print $ prod2Tfs sessionTf1 sessionTf2
