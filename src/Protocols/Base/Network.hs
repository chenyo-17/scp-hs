module Protocols.Base.Network where

import           Functions.Transfer
import           Protocols.Base.Router

newtype NetProtoTf =
  NetProtoTf Tf
  deriving (Eq)

-- given a list of router proto tfs of the same protocol,
-- convert them to a net proto tf
toNetProtoTf :: [RouterProtoTf] -> NetProtoTf
toNetProtoTf = undefined
