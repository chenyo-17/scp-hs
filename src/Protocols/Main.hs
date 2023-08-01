module Main where

import           Functions.Transfer
import           Protocols.BGP
import           Protocols.Protocol
import           Utilities.Ip

main :: IO ()
main = do
  let ip1 = read "17.0.0.0/8" :: IpPrefix
  let ip2 = read "19.0.0.0/8" :: IpPrefix
  let pl1 = [ip1, ip2]
  let cl1 = [1200, 1201]
  let rmItem1 = RmItem Permit [MatchIpPrefix pl1] [SetLocalPref 150]
  let rmItem2 = RmItem Deny [MatchCommunity cl1] []
  let rm1 = BgpRm [rmItem1]
  let rm2 = BgpRm [rmItem2]
  putStrLn $ "BGP rm: " ++ show rm1
  putStrLn $ "BGP rmTf:\n" ++ show (toTf rm1)
