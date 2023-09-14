module Functions.Transfer where

import           Control.Parallel            (par, pseq)
import           Control.Parallel.Strategies
import qualified Data.HashMap.Strict         as HashMap
import           Data.List                   (intercalate, foldl')
import           Data.List.Split
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Word
import           Utilities.Parallel

data TfExpr
  = TfVar String
  | TfConst TfLiteral
  | TfAdd TfExpr TfExpr
  deriving (Eq)

-- logical operators
data TfOpA
  = TfGe
  | TfLe
  | TfGt
  | TfLt
  | TfEq
  | TfNe
  deriving (Eq)

-- reverse the operator
reverseOpA :: TfOpA -> TfOpA
reverseOpA op =
  case op of
    TfGe -> TfLt
    TfLe -> TfGt
    TfGt -> TfLe
    TfLt -> TfGe
    TfEq -> TfNe
    TfNe -> TfEq

data TfLiteral
  = TfInt Word32
  | TfBool Bool
  | TfNull
  -- | TfString String
  -- | TfPattern String -- regular expression
  deriving (Eq)

-- TODO: implement Eq, e.g., TfOr a b == TfOr b a
data TfCondition
  = TfCond TfExpr TfOpA TfExpr
  | TfTrue
  | TfFalse
  | TfNot TfCondition
  | TfAnd TfCondition TfCondition
  | TfOr TfCondition TfCondition
  | TfImply TfCondition TfCondition
  deriving (Eq)

-- the key is TfVar
data TfAssignItem = TfAssignItem
  { assignVar   :: TfExpr
  , assignValue :: TfExpr
  } deriving (Eq)

data TfAssign
  = TfAssign [TfAssignItem]
  | TfAssignNull -- null assignment
  deriving (Eq)

-- an assign is null if it is TfAssignNull or all its items are null
isNullAssign :: TfAssign -> Bool
isNullAssign assign =
  case assign of
    TfAssign items -> all (\item -> assignValue item == TfConst TfNull) items
    TfAssignNull   -> True

-- lookup an assign value by a variable
getAssignVal :: TfExpr -> TfAssign -> Maybe TfExpr
getAssignVal var assign =
  case assign of
    TfAssign items ->
      lookup var $ map (\item -> (assignVar item, assignValue item)) items
    TfAssignNull -> Nothing

data TfClause = TfClause
  { tfCond   :: TfCondition
  , tfAssign :: TfAssign
  } deriving (Eq)

newtype Tf = Tf
  { tfClauses :: [TfClause]
  } deriving (Eq)

-- add a TfAssignItem to a TfAssign
addTfAssignItem :: TfExpr -> TfExpr -> TfAssign -> TfAssign
addTfAssignItem var value assign =
  case assign of
    TfAssign items -> TfAssign $ TfAssignItem var value : items
    TfAssignNull   -> TfAssignNull

-- convert a TfAssign to a TfCondition by equaling the var and value
assignToCond :: TfAssign -> TfCondition
assignToCond TfAssignNull = TfFalse
assignToCond (TfAssign items) = foldr concatAssign TfTrue items
  where
    concatAssign :: TfAssignItem -> TfCondition -> TfCondition
    concatAssign _ TfFalse = TfFalse
    concatAssign (TfAssignItem var val) cond =
      simplifyCond $ TfAnd cond (TfCond var TfEq val)

-- simplify one TfCondition
-- TODO: cannot detect a == b && b == a are the same
-- TODO: cannot detect a == c && a == b is false
-- TODO: one way is to go through all Eq for a when adding a new Eq regarding a
simplifyCond :: TfCondition -> TfCondition
simplifyCond cond =
  case cond of
    TfAnd TfTrue c1 -> simplifyCond c1
    TfAnd c1 TfTrue -> simplifyCond c1
    TfAnd TfFalse _ -> TfFalse
    TfAnd _ TfFalse -> TfFalse
    TfAnd c1 c2
      | c1 == c2 -> c1
    -- TfAnd c1 c2 -> TfAnd (simplifyCond c1) (simplifyCond c2)
    -- if one of the simplified condition if TfFalse or TfTrue, then they remain
    -- so need to simplify again based on the simplified condition
    TfAnd c1 c2 ->
      let c1' = simplifyCond c1
          c2' = simplifyCond c2
       in case (c1', c2') of
            (TfTrue, _)  -> c2'
            (_, TfTrue)  -> c1'
            (TfFalse, _) -> TfFalse
            (_, TfFalse) -> TfFalse
            _            -> TfAnd c1' c2'
    TfOr TfTrue _ -> TfTrue
    TfOr _ TfTrue -> TfTrue
    TfOr TfFalse c1 -> simplifyCond c1
    TfOr c1 TfFalse -> simplifyCond c1
    TfOr c1 c2
      | c1 == c2 -> c1
    -- TfOr c1 c2 -> TfOr (simplifyCond c1) (simplifyCond c2)
    TfOr c1 c2 ->
      let c1' = simplifyCond c1
          c2' = simplifyCond c2
       in case (c1', c2') of
            (TfTrue, _)  -> TfTrue
            (_, TfTrue)  -> TfTrue
            (TfFalse, _) -> c2'
            (_, TfFalse) -> c1'
            _            -> TfOr c1' c2'
    TfNot TfTrue -> TfFalse
    TfNot TfFalse -> TfTrue
    TfNot (TfNot c1) -> simplifyCond c1
    TfNot (TfCond e1 op e2) -> simplifyCond $ TfCond e1 (reverseOpA op) e2
    -- TfNot (TfAnd c1 c2) ->
    --   TfOr (simplifyCond $ TfNot c1) (simplifyCond $ TfNot c2)
    TfNot (TfAnd c1 c2) ->
      let c1' = simplifyCond $ TfNot c1
          c2' = simplifyCond $ TfNot c2
       in case (c1', c2') of
            (TfTrue, _) -> TfTrue
            (_, TfTrue) -> TfTrue
            (TfFalse, _) -> c2'
            (_, TfFalse) -> c1'
            _
              | c1' == c2' -> c1'
            _ -> TfOr c1' c2'
    -- TfNot (TfOr c1 c2) ->
    --   TfAnd (simplifyCond $ TfNot c1) (simplifyCond $ TfNot c2)
    TfNot (TfOr c1 c2) ->
      let c1' = simplifyCond $ TfNot c1
          c2' = simplifyCond $ TfNot c2
       in case (c1', c2') of
            (TfTrue, _) -> c2'
            (_, TfTrue) -> c1'
            (TfFalse, _) -> TfFalse
            (_, TfFalse) -> TfFalse
            _
              | c1' == c2' -> c1'
            _ -> TfAnd c1' c2'
    -- compute simple arithmetic expression
    -- TODO: also consider other literals, e.g., strings, patterns
    TfCond (TfConst (TfInt i1)) op (TfConst (TfInt i2)) ->
      case op of
        TfGe ->
          if i1 >= i2
            then TfTrue
            else TfFalse
        TfLe ->
          if i1 <= i2
            then TfTrue
            else TfFalse
        TfGt ->
          if i1 > i2
            then TfTrue
            else TfFalse
        TfLt ->
          if i1 < i2
            then TfTrue
            else TfFalse
        TfEq ->
          if i1 == i2
            then TfTrue
            else TfFalse
        TfNe ->
          if i1 /= i2
            then TfTrue
            else TfFalse
    TfCond e1 op e2
      | e1 == e2 ->
        case op of
          TfEq -> TfTrue
          TfNe -> TfFalse
          TfGe -> TfTrue
          TfLe -> TfTrue
          TfGt -> TfFalse
          TfLt -> TfFalse
    TfCond (TfConst TfNull) op e2 ->
      case (op, e2) of
        -- cannot compare Null with a variable
        (TfEq, TfConst _) -> TfFalse
        (TfNe, TfConst _) -> TfTrue
        (TfGe, TfConst _) -> TfFalse
        (TfLe, _)         -> TfTrue
        (TfGt, _)         -> TfFalse
        (TfLt, TfConst _) -> TfTrue
        _                 -> cond
    TfCond e1 op (TfConst TfNull) ->
      case (e1, op) of
        (TfConst _, TfEq) -> TfFalse
        (TfConst _, TfNe) -> TfTrue
        (_, TfGe)         -> TfTrue
        (TfConst _, TfLe) -> TfFalse
        (TfConst _, TfGt) -> TfTrue
        (_, TfLt)         -> TfFalse
        _                 -> cond
    TfCond (TfAdd (TfConst (TfInt i1)) (TfConst (TfInt i2))) op e2 ->
      case op of
        TfEq -> simplifyCond $ TfCond (TfConst (TfInt (i1 + i2))) TfEq e2
        TfNe -> simplifyCond $ TfCond (TfConst (TfInt (i1 + i2))) TfNe e2
        TfGe -> simplifyCond $ TfCond (TfConst (TfInt (i1 + i2))) TfGe e2
        TfLe -> simplifyCond $ TfCond (TfConst (TfInt (i1 + i2))) TfLe e2
        TfGt -> simplifyCond $ TfCond (TfConst (TfInt (i1 + i2))) TfGt e2
        TfLt -> simplifyCond $ TfCond (TfConst (TfInt (i1 + i2))) TfLt e2
    TfImply c1 c2 -> simplifyCond $ TfOr (TfNot c1) (TfAnd c1 c2)
    TfNot (TfImply c1 c2) -> simplifyCond $ TfAnd c1 (TfNot c2)
    _ -> cond

-- assignMap :: [TfAssignItem] -> Map.Map String TfExpr
-- assignMap items =
--   Map.fromList $ concat $ parMap rseq convertChunk (chunksOf chunkSize items)
--   where
--     convertChunk = mapMaybe convert
--     convert (TfAssignItem (TfVar v) e) = Just (v, e)
--     convert _                          = Nothing
-- -- assignMap :: [TfAssignItem] -> Map.Map String TfExpr
-- -- assignMap = Map.fromList . map (\(TfAssignItem (TfVar v) e) -> (v, e))
-- substEachOpt :: Map.Map String TfExpr -> TfCondition -> TfCondition
-- substEachOpt _ TfFalse = TfFalse
-- substEachOpt m (TfCond (TfVar v1) op e2) =
--   case Map.lookup v1 m of
--     Just e  -> TfCond e op e2
--     Nothing -> TfCond (TfVar v1) op e2
-- substEachOpt m (TfCond e1 op (TfVar v2)) =
--   case Map.lookup v2 m of
--     Just e  -> TfCond e1 op e
--     Nothing -> TfCond e1 op (TfVar v2)
-- substEachOpt m (TfNot c) = TfNot (substEachOpt m c)
-- substEachOpt m (TfAnd c1 c2) = TfAnd (substEachOpt m c1) (substEachOpt m c2)
-- substEachOpt m (TfOr c1 c2) = TfOr (substEachOpt m c1) (substEachOpt m c2)
-- substEachOpt m (TfImply c1 c2) = TfImply (substEachOpt m c1) (substEachOpt m c2)
-- substEachOpt _ cond' = cond'
-- substCond :: TfCondition -> TfAssign -> TfCondition
-- substCond _ TfAssignNull = TfFalse
-- substCond cond (TfAssign as) =
--   let m = assignMap as
--    in simplifyCond $ substEachOpt m cond
substCond :: TfCondition -> TfAssign -> TfCondition
substCond _ TfAssignNull = TfFalse
substCond cond (TfAssign as) =
  assignMap
    `par` (substFromMap cond assignMap
             `pseq` simplifyCond (substFromMap cond assignMap))
  where
    -- Create a map from the assignment for O(1) lookups.
    -- assignMap = Map.fromList [(v, e) | TfAssignItem (TfVar v) e <- as]
    toPair (TfAssignItem (TfVar v) e) = (v, e)
    assignMap =
      foldl'
        (\acc item -> Map.insert (fst item) (snd item) acc)
        Map.empty
        (map toPair as)
    substFromMap :: TfCondition -> Map.Map String TfExpr -> TfCondition
    substFromMap TfFalse _ = TfFalse
    substFromMap (TfCond (TfVar v) op e2) m
      | Just expr <- Map.lookup v m = TfCond expr op e2
    substFromMap (TfCond e1 op (TfVar v)) m
      | Just expr <- Map.lookup v m = TfCond e1 op expr
    substFromMap (TfNot c) m = TfNot (substFromMap c m)
    substFromMap (TfAnd c1 c2) m = TfAnd (substFromMap c1 m) (substFromMap c2 m)
    substFromMap (TfOr c1 c2) m = TfOr (substFromMap c1 m) (substFromMap c2 m)
    substFromMap (TfImply c1 c2) m =
      TfImply (substFromMap c1 m) (substFromMap c2 m)
    substFromMap c _ = c -- If no substitution, return the condition unchanged.

-- -- substitute the variable in the condition with the assign
-- -- and simplify the condition
-- -- TODO: support numeric conditions, e.g., a + 10 > 20 && a < 10
-- substCond :: TfCondition -> TfAssign -> TfCondition
-- -- if the assign of export (first clause) is null
-- -- then the new condition is always false
-- substCond _ TfAssignNull = TfFalse
-- -- not use map as it has sharing
-- substCond cond (TfAssign as) = simplifyCond $ foldr substEach cond as
--   where
--     -- substitute all instances of v in cond
--     substEach :: TfAssignItem -> TfCondition -> TfCondition
--     substEach _ TfFalse = TfFalse
--     substEach a@(TfAssignItem (TfVar v) e) cond' =
--       case cond' of
--         -- the variable can only appears in TfCond as a single var
--         TfCond (TfVar v') op e2
--           | v == v' -> TfCond e op e2
--         TfCond e1 op (TfVar v')
--           | v == v' -> TfCond e1 op e
--         TfNot c -> TfNot (substEach a c)
--         TfAnd c1 c2 -> TfAnd (substEach a c1) (substEach a c2)
--         TfOr c1 c2 -> TfOr (substEach a c1) (substEach a c2)
--         TfImply c1 c2 -> TfImply (substEach a c1) (substEach a c2)
--         _ -> cond'
--     -- the key in an tfAssign must be a var
--     substEach _ cond' = cond'
-- given a tf clause, convert its assign to condition and concat it with condition
-- e.g., a := b -> a == b
-- also substitute the condition with the assign in best effort to simplify it
clauseToTfCond :: TfClause -> TfCondition
clauseToTfCond (TfClause cond assign) = newCond
  where
    cond' = substCond cond assign
    newCond = simplifyCond $ TfAnd cond' (assignToCond assign)

-- concat the tf conditions and assigns of 2 clauses
-- no substitution is performed
concatTfClauses :: TfClause -> TfClause -> TfClause
concatTfClauses (TfClause c1 a1) (TfClause c2 a2) = TfClause c' a'
  where
    -- combine two conditions
    c' = simplifyCond $ TfAnd c1 c2
    -- combine two assigns
    a' = concat2Assigns a1 a2

-- combine two assigns, assume there is no duplicate keys
concat2Assigns :: TfAssign -> TfAssign -> TfAssign
-- if some assign is null, return the other one
-- this is required when concatenating a list of assigns with fold
concat2Assigns TfAssignNull a = a
concat2Assigns a TfAssignNull = a
-- the result cannot be null, as the null case has been handled
concat2Assigns a1 a2 = TfAssign assignList
  where
    -- concat two assign lists into one,
    -- each element is an TfAssignItem
    -- the second is appended to the first one
    assignList =
      let TfAssign l1 = a1
          TfAssign l2 = a2
       in l1 ++ l2

-- append a suffix to all TfVar in the condition
appendCondVar :: String -> TfCondition -> TfCondition
appendCondVar str cond =
  case cond of
    TfCond e1 op e2 -> TfCond (appendExprVar str e1) op (appendExprVar str e2)
    TfNot c         -> TfNot $ appendCondVar str c
    TfAnd c1 c2     -> TfAnd (appendCondVar str c1) (appendCondVar str c2)
    TfOr c1 c2      -> TfOr (appendCondVar str c1) (appendCondVar str c2)
    TfImply c1 c2   -> TfImply (appendCondVar str c1) (appendCondVar str c2)
    _               -> cond

-- append a suffix to all values in the assign
appendAssignVal :: String -> TfAssign -> TfAssign
appendAssignVal _ TfAssignNull = TfAssignNull
appendAssignVal str (TfAssign as) =
  TfAssign
    $ map (\(TfAssignItem v e) -> TfAssignItem v (appendExprVar str e)) as

-- append a suffix to all TfVar in the assign
appendAssignVar :: String -> TfAssign -> TfAssign
appendAssignVar _ TfAssignNull = TfAssignNull
appendAssignVar str (TfAssign as) =
  TfAssign
    $ map (\(TfAssignItem v e) -> TfAssignItem (appendExprVar str v) e) as

-- append a suffix to all TfVar in the expression
appendExprVar :: String -> TfExpr -> TfExpr
appendExprVar str expr =
  case expr of
    TfVar v     -> TfVar $ v ++ str
    TfAdd e1 e2 -> TfAdd (appendExprVar str e1) (appendExprVar str e2)
    _           -> expr

type TfExprPair = (TfExpr, TfExpr)

-- convert a list of tf assign items to an association list
-- where the key is the variable and the value is the value
toAssList :: [TfAssignItem] -> [TfExprPair]
toAssList = map (\item -> (assignVar item, assignValue item))

-- convert an association list to a list of tf assign items
fromAssList :: [TfExprPair] -> [TfAssignItem]
fromAssList = map (uncurry TfAssignItem)

-- given a tf clause, substitute each assign value to the eventual unwrapped value
-- e.g., {a := b, b := c, c := 10} -> {a := 10, b := 10, c := 10}
-- if a value is already a constant, or it is a var that does not appear in the assign,
-- then it is unwrapped. Otherwise always unwrap the value as deep as it could be
-- Note that this function assumes there is no cyclic dependencies
unwrapAssign :: TfAssign -> TfAssign
unwrapAssign TfAssignNull = TfAssignNull
unwrapAssign (TfAssign oldAssign) =
  TfAssign (fromAssList $ foldr unwrapItem [] oldList)
  where
    oldList = toAssList oldAssign
    unwrapItem :: (TfExpr, TfExpr) -> [(TfExpr, TfExpr)] -> [(TfExpr, TfExpr)]
    -- first check whether the key is already in the acc list
    -- which is possible as when recursively unwrapping, some keys may already be unwrapped
    -- even if it has not been visited
    unwrapItem (var, value) newList =
      case lookup var newList of
        Just _ -> newList
        Nothing -> (var, newValue) : newList'
          where (newValue, newList') = unwrapValue value newList
          -- given a tf expr and a list of unwrapped values, unwrap the expr
          -- and update the list during the recursive unwrapping
                unwrapValue ::
                     TfExpr
                  -> [(TfExpr, TfExpr)]
                  -> (TfExpr, [(TfExpr, TfExpr)])
                unwrapValue (TfConst lit) acc = (TfConst lit, acc)
                unwrapValue v@(TfVar _) acc =
                  case lookup v oldList of
            -- the variable is an environment variable
                    Nothing  -> (v, acc)
                    Just val -> unwrapValue val acc
                unwrapValue (TfAdd e1 e2) acc = (TfAdd e1' e2', acc'')
                  where
                    (e1', acc') = unwrapValue e1 acc
                    (e2', acc'') = unwrapValue e2 acc'

instance Show TfOpA where
  show op =
    case op of
      TfGe -> " >= "
      TfLe -> " <= "
      TfGt -> " > "
      TfLt -> " < "
      TfEq -> " == "
      TfNe -> " != "

instance Show TfLiteral where
  show lit =
    case lit of
      TfInt i  -> show i
      TfBool b -> show b
      TfNull   -> "null"
      -- TfString s  -> show s
      -- TfPattern p -> show p

instance Show TfExpr where
  show expr =
    case expr of
      TfVar v     -> v
      TfConst lit -> show lit
      TfAdd e1 e2 -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"

instance Show TfCondition where
  show cond =
    case cond of
      TfCond e1 op e2 -> show e1 ++ show op ++ show e2
      TfTrue -> show True
      TfFalse -> show False
      TfNot c -> "not " ++ "(" ++ show c ++ ")"
      TfAnd c1 c2 -> "(" ++ show c1 ++ ")" ++ " & " ++ "(" ++ show c2 ++ ")"
      TfOr c1 c2 -> "(" ++ show c1 ++ ")" ++ " | " ++ "(" ++ show c2 ++ ")"
      TfImply c1 c2 -> "(" ++ show c1 ++ ")" ++ " => " ++ "(" ++ show c2 ++ ")"

instance Show TfAssignItem where
  show (TfAssignItem e1 e2) = show e1 ++ " := " ++ show e2

instance Show TfAssign where
  show assign =
    case assign of
    -- use , to separate items
      TfAssign items -> intercalate ", " $ map show items
      TfAssignNull   -> "Null"

instance Show TfClause where
  show (TfClause cond assign) = show cond ++ " -> " ++ show assign

instance Show Tf where
  show (Tf clauses) = unlines $ map show clauses
