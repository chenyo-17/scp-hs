module Functions.Transfer where

import           Data.List (foldl', intercalate)
import           Data.Word

data TfExpr
  = TfVar String
  | TfConst TfLiteral
  | TfNull
  deriving (Eq)

data TfOp
  = TfGe
  | TfLe
  | TfGt
  | TfLt
  | TfEq
  | TfNe
  deriving (Eq)

-- reverse the operator
reverseOp :: TfOp -> TfOp
reverseOp op =
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
  | TfString String
  | TfPattern String -- regular expression
  deriving (Eq)

data TfCondition
  = TfCond TfExpr TfOp TfExpr
  | TfTrue
  | TfFalse
  | TfNot TfCondition
  | TfAnd TfCondition TfCondition
  | TfOr TfCondition TfCondition
  deriving (Eq)

-- the key is CVar
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
    TfAssign items -> all (\item -> assignValue item == TfNull) items
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
    TfNot (TfCond e1 op e2) -> simplifyCond $ TfCond e1 (reverseOp op) e2
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
    TfCond TfNull op _ ->
      case op of
        TfEq -> TfFalse
        TfNe -> TfTrue
        TfGe -> TfFalse
        TfLe -> TfTrue
        TfGt -> TfFalse
        TfLt -> TfTrue
    TfCond _ op TfNull ->
      case op of
        TfEq -> TfFalse
        TfNe -> TfTrue
        TfGe -> TfTrue
        TfLe -> TfFalse
        TfGt -> TfTrue
        TfLt -> TfFalse
    _ -> cond

-- substitute the variable in the condition with the assign
-- and simplify the condition
-- TODO: support numeric conditions, e.g., a + 10 > 20 && a < 10
substCond :: TfCondition -> TfAssign -> TfCondition
-- if the assign of export (first clause) is null
-- then the new condition is always false
substCond _ TfAssignNull = TfFalse
-- not use map as it has sharing
substCond cond (TfAssign as) = simplifyCond $ foldr substEach cond as
  where
    -- substitute all instances of v in cond
    substEach :: TfAssignItem -> TfCondition -> TfCondition
    substEach _ TfFalse = TfFalse
    substEach a@(TfAssignItem (TfVar v) e) cond' =
      case cond' of
        -- the variable can only appears in TfCond as a single var
        TfCond (TfVar v') op e2
          | v == v' -> TfCond e op e2
        TfCond e1 op (TfVar v')
          | v == v' -> TfCond e1 op e
        TfNot c -> TfNot $ substEach a c
        TfAnd c1 c2 -> TfAnd (substEach a c1) (substEach a c2)
        TfOr c1 c2 -> TfOr (substEach a c1) (substEach a c2)
        _ -> cond'
    -- the key in an tfAssign must be a var
    substEach _ cond' = cond'

-- concatenate two TfClauses to one TfClause
-- the check of the second clause cond is based on the first clause's assign
-- e.g.1, c1 = a > 10 -> a := 5
--        c2 =  a < 6 -> b := 3
--    returns c3 = a > 10 -> a := 5, b := 3
-- e.g.2, c1 = a > 10 -> b := 10
--        c2 = a < 6 -> a := 5
--    returns c3 = a > 10 -> b := 10, a := 5
-- the second assign overrides the first one if there assign the same variable
concatTfClauses :: (TfClause, TfClause) -> Maybe TfClause
concatTfClauses (TfClause c1 a1, TfClause c2 a2) =
  if c' == TfFalse
    then Nothing
    else Just $ TfClause c' a'
  where
    -- combine two conditions
    c' = simplifyCond $ TfAnd c1 (substCond c2 a1)
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
    TfVar v -> TfVar $ v ++ str
    _       -> expr

instance Show TfOp where
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
      TfInt i     -> show i
      TfBool b    -> show b
      TfString s  -> show s
      TfPattern p -> show p

instance Show TfExpr where
  show expr =
    case expr of
      TfVar v     -> v
      TfConst lit -> show lit
      TfNull      -> "null"

instance Show TfCondition where
  show cond =
    case cond of
      TfCond e1 op e2 -> show e1 ++ show op ++ show e2
      TfTrue -> show True
      TfFalse -> show False
      TfNot c -> "not " ++ "(" ++ show c ++ ")"
      TfAnd c1 c2 -> "(" ++ show c1 ++ ")" ++ " && " ++ "(" ++ show c2 ++ ")"
      TfOr c1 c2 -> "(" ++ show c1 ++ ")" ++ " || " ++ "(" ++ show c2 ++ ")"

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
