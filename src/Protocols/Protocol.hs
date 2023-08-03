module Protocols.Protocol where

import           Data.List
import           Data.Maybe         (fromMaybe)
import           Data.Word
import           Functions.Transfer

type RouterId = Word32

data SessionDir
  = Import
  | Export
  deriving (Show, Eq)

-- reverse the session direction
reverseDir :: SessionDir -> SessionDir
reverseDir dir =
  case dir of
    Import -> Export
    Export -> Import

-- reverse the session
reverseSs :: Session -> Session
reverseSs (Session src dir dst) = Session dst (reverseDir dir) src

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
      Import -> show src ++ " <- " ++ show dst
      Export -> show src ++ " -> " ++ show dst

-- (a, b) means this is a link for a to receive b's routes
-- it is associated with a tf that chaining tf of session (b, export, a)
-- and tf of session (a, import, b)
data Link = Link
  { lkSrc :: RouterId
  , lkDst :: RouterId
  } deriving (Eq)

instance Show Link where
  show (Link src dst) = show src ++ " <- " ++ show dst

class Route a where
  mergeRoute :: a -> a -> Maybe a

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
    show ss ++ " " ++ show proto ++ ":\n" ++ show tf

-- customized show for [SessionProtoTf]
showSessionTfs :: [SessionProtoTf] -> String
showSessionTfs = intercalate "\n" . map show

data LinkProtoTf = LinkProtoTf
  { link   :: Link
  , lProto :: Protocol
  , lTf    :: Tf
  } deriving (Eq)

instance Show LinkProtoTf where
  show (LinkProtoTf lk proto tf) =
    show lk ++ " " ++ show proto ++ ":\n" ++ show tf

-- customized show for [LinkProtoTf]
showLinkTfs :: [LinkProtoTf] -> String
showLinkTfs = intercalate "\n" . map show

class SessionTf a where
  addSessionTf :: Session -> a -> [SessionProtoTf] -> [SessionProtoTf]
  -- user API to convert a list of (session, a) pairs to session tfs
  addSessionTfs :: [(Session, a)] -> [SessionProtoTf]
  addSessionTfs = foldr (uncurry addSessionTf) []
  -- user API to convert a list of (session, a) pairs to link tfs
  addLinkTfs :: [(Session, a)] -> [LinkProtoTf]
  addLinkTfs = toLinkTfs . addSessionTfs

-- convert session tfs to link tfs
-- by chaining sessionProtoTf (b export a) and (a import b) to linkProtoTf (a b)
toLinkTfs :: [SessionProtoTf] -> [LinkProtoTf]
toLinkTfs ssTfs = foldr toLinkTf [] ssTfs
  where
    -- update the link tfs by adding a new session tf
    toLinkTf :: SessionProtoTf -> [LinkProtoTf] -> [LinkProtoTf]
    -- given a sessionTf, first check whether its other part is already processed
    -- if so, do nothing, otherwise, first find the other part, then chain them
    toLinkTf sTf@(SessionProtoTf ssa@(Session _ sDir _) proto _) linkTfs
      | isProcessed = linkTfs -- the pair of sTf is already processed
      | sDir == Export -- decide the order of concatenation
       = concatSsTfs sTf pairSsTf : linkTfs
      | otherwise = concatSsTfs pairSsTf sTf : linkTfs
        -- compute the pair session
      where
        ssb = reverseSs ssa
        -- prepare the default sessionTf
        defaultSessionTf = SessionProtoTf ssb proto defaultTf
        -- check whether sTf is already processed
        isProcessed :: Bool
        isProcessed =
          any (\(LinkProtoTf l p _) -> p == proto && l == l') linkTfs
          where
            l' =
              if sDir == Import
                then Link (ssSrc ssa) (ssDst ssa)
                else Link (ssDst ssa) (ssSrc ssa)
        -- get the pair of ssTf in the ssTfs
        pairSsTf :: SessionProtoTf
        pairSsTf =
          fromMaybe defaultSessionTf
            $ find
                (\(SessionProtoTf ssb' proto' _) ->
                   ssb == ssb' && proto == proto')
                ssTfs
        -- concatenate a pair of session tfs to one link tf
        -- the first is export tf, the second is import tf
        concatSsTfs :: SessionProtoTf -> SessionProtoTf -> LinkProtoTf
        concatSsTfs ssTfe ssTfi = LinkProtoTf l p linkTf
          where
            ssImp = session ssTfi
            -- the link refers to the import session
            l = Link (ssSrc ssImp) (ssDst ssImp)
            p = ssProto ssTfi
            linkTf = foldr concatClauses (Tf []) (prod2Tfs ssTfe ssTfi)
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
        -- produce the clause product of 2 tfs
        prod2Tfs :: SessionProtoTf -> SessionProtoTf -> [(TfClause, TfClause)]
        prod2Tfs (SessionProtoTf _ _ tf1) (SessionProtoTf _ _ tf2) =
          [(c1, c2) | c1 <- tfClauses tf1, c2 <- tfClauses tf2]
