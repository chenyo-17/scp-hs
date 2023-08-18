module Utilities.Ip where

import           Data.Bits
import           Data.IP
import           Data.Word

type IpPrefix = AddrRange IPv4

ipPrefix :: String -> IpPrefix
ipPrefix str = read str :: IpPrefix

type Ip = IPv4

ip :: String -> Ip
ip str = read str :: Ip

type IpRangew = (Word32, Word32)

-- convert a IpPrefix to its integer range
toIpRangew :: IpPrefix -> IpRangew
toIpRangew prefix = (ipLow, ipHiw)
  where
    (ipLo, maskLen) = addrRangePair prefix
    ipLow = fromIPv4w ipLo
    ipHiw = ipLow .|. maxBoundAddr maskLen
      where
        maxBoundAddr :: Int -> Word32
        maxBoundAddr len = 2 ^ (32 - len) - 1

-- convert an ip address to integer
fromIpw :: IPv4 -> Word32
fromIpw = fromIPv4w
