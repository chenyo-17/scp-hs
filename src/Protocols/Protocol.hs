module Protocols.Protocol where

import           Data.Word
import           Functions.Transfer

type RouterId = Word32

data SessionDir
  = Import
  | Export
  deriving (Show, Eq)

-- a Import b means this is an import session at a to apply on b
-- a Export b means this is an export session at a to apply on b
data Session = Session
  { ssSrc :: RouterId
  , ssDir :: SessionDir
  , ssDst :: RouterId
  } deriving (Eq)

instance Show Session where
  show (Session src dir dst) =
    case dir of
      Import -> show src ++ "<-" ++ show dst
      Export -> show src ++ "->" ++ show dst

-- (a, b) means this is a link for a to receive b's routes
-- it is associated with a tf that chaining tf of session (b, export, a)
-- and tf of session (a, import, b)
data Link = Link
  { lkSrc :: RouterId
  , lkDst :: RouterId
  } deriving (Eq)

instance Show Link where
  show (Link src dst) = show src ++ "<-" ++ show dst

data Protocol =
  BGP
  deriving (Show, Eq)

data SessionProtoTf = SessionProtoTf
  { session :: Session
  , ssProto :: Protocol
  , ssTf    :: Tf
  } deriving (Eq)

-- TODO: automatically compute indentation
instance Show SessionProtoTf where
  show (SessionProtoTf ss proto tf) =
    "sessionTf " ++ show ss ++ " " ++ show proto ++ ":\n" ++ show tf

data LinkProtoTf = LinkProtoTf
  { link   :: Link
  , lProto :: Protocol
  , lTf    :: Tf
  } deriving (Eq)

instance Show LinkProtoTf where
  show (LinkProtoTf lk proto tf) =
    "linkTf " ++ show lk ++ " " ++ show proto ++ ":\n" ++ show tf

-- a session function is a pair of session and a,
-- where a is an instance of SessionTf
type SessionF a = (Session, a)

-- pair of session functions, the first is export tf,
-- the second is import tf
type SessionFPair a = (SessionF a, SessionF a)

class Route a where
  -- return the better route if two routes are comparable
  mergeRoute :: a -> a -> Maybe a

class SessionTf a where
  -- given a session functions, convert to a session tf
  toSessionTf :: SessionF a -> SessionProtoTf
  -- given a pair of session functions, convert to a link tf
  -- TODO: finding the right pair of session tfs
  -- is the responsibility of the config parser
  -- the goal is to compute each router's node tf concurrently with lazy evaluation
  -- e.g., a link tf is only computed when the network tf requires
  toLinkTf :: SessionFPair a -> LinkProtoTf
  toLinkTf (sfe, sfi@(ssi, _)) = LinkProtoTf l p linkTf
    where
      sTfe = toSessionTf sfe
      sTfi = toSessionTf sfi
      l = Link (ssSrc ssi) (ssDst ssi)
      p = ssProto sTfe
      linkTf = foldr concatClauses (Tf []) (prod2Tfs (ssTf sTfe) (ssTf sTfi))
        where
          concatClauses :: (TfClause, TfClause) -> Tf -> Tf
          -- if 2 clauses can concat, add it to the new tf
          -- otherwise, do nothing
          concatClauses (c1, c2) tf =
            case c' of
              Just c  -> Tf (c : tfClauses tf)
              Nothing -> tf
            where
              c' = concatTfClause (c1, c2)

-- reverse the session direction
reverseDir :: SessionDir -> SessionDir
reverseDir dir =
  case dir of
    Import -> Export
    Export -> Import

-- reverse the session
reverseSs :: Session -> Session
reverseSs (Session src dir dst) = Session dst (reverseDir dir) src
