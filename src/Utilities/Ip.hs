module Utilities.Ip where

import           Data.IP
import           Data.Word
import Data.Bits

type IpPrefix = AddrRange IPv4

type Ip = IPv4

type IpRangew = (Word32, Word32)

-- convert a IpPrefix to its integer range
toIpRangew :: IpPrefix -> IpRangew
toIpRangew ipPrefix = (ipLow, ipHiw)
  where
    (ipLo, maskLen) = addrRangePair ipPrefix
    ipLow = fromIPv4w ipLo
    ipHiw = ipLow .|. maxBoundAddr maskLen
      where
        maxBoundAddr :: Int -> Word32
        maxBoundAddr len = 2 ^ (32 - len) - 1

-- convert an ip address to integer
fromIpw :: IPv4 -> Word32
fromIpw = fromIPv4w
