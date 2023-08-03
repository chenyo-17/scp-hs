module Functions.Transfer where

import           Data.List (intercalate)
import           Data.Word

data TfExpr
  = TfVar String
  | TfConst TfLiteral
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
data TfAssignItem =
  TfAssignItem TfExpr TfExpr
  deriving (Eq)

data TfAssign
  = TfAssign [TfAssignItem]
  | TfAssignNull -- null assignment
  deriving (Eq)

data TfClause = TfClause
  { tfCond   :: TfCondition
  , tfAssign :: TfAssign
  } deriving (Eq)

newtype Tf = Tf
  { tfClauses :: [TfClause]
  } deriving (Eq)

class Transfer a where
  toTf :: a -> Tf
  toSimpleTf :: a -> Tf
  toSimpleTf = simplifyTf . toTf
      -- simplify each Tf clause condition
    where
      simplifyTf :: Tf -> Tf
      -- not use foldr as Tf is not a list type
      simplifyTf (Tf []) = Tf []
      -- this ugly cs is because Tf must be a newtype
      simplifyTf (Tf (c:cs)) = Tf (c' : (tfClauses . simplifyTf . Tf) cs)
        where
          c' = TfClause (simplifyCond $ tfCond c) (tfAssign c)

-- default Tf does not change anything
defaultTf :: Tf
defaultTf = Tf [trueClause]
  where
    trueClause = TfClause TfTrue (TfAssign [])

-- simplify one TfCondition
-- TODO: cannot detect a == b && b == a are the same
-- TODO: cannot detect a == c && a == b is false
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
    _ -> cond

-- produce the clause product of 2 tfs
prod2Tfs :: Tf -> Tf -> [(TfClause, TfClause)]
prod2Tfs tf1 tf2 = [(c1, c2) | c1 <- tfClauses tf1, c2 <- tfClauses tf2]

-- concatenate two TfClauses to one TfClause
-- the check of the second clause cond is based on the first clause's assign
-- e.g.1, c1 = a > 10 -> a := 5
--        c2 =  a < 6 -> b := 3
--    returns c3 = a > 10 -> a := 5, b := 3
-- e.g.2, c1 = a > 10 -> b := 10
--        c2 = a < 6 -> a := 5
--    returns c3 = a > 10 -> b := 10, a := 5
-- the second assign overrides the first one if there assign the same variable
concatTfClause :: (TfClause, TfClause) -> Maybe TfClause
concatTfClause (TfClause c1 a1, TfClause c2 a2) =
  if c' == TfFalse
    then Nothing
    else Just $ TfClause c' a'
  where
    -- combine two conditions
    c' = simplifyCond $ TfAnd c1 (substCond c2 a1)
    -- combine two assigns
    a' = comb2Assigns a1 a2

-- substitute the variable in the condition with the assign
-- and simplify the condition
-- TODO: support numeric conditions, e.g., a + 10 > 20 && a < 10
substCond :: TfCondition -> TfAssign -> TfCondition
-- if the assign of export (first clause) is null
-- then the new condition is always false
substCond _ TfAssignNull = TfFalse
substCond cond (TfAssign as) = simplifyCond $ foldr substEach cond as
  where
    -- substitute all instances of v in cond
    substEach :: TfAssignItem -> TfCondition -> TfCondition
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

-- combine two assigns, the second one overrides the first one
-- if there is null, the null is propagated
comb2Assigns :: TfAssign -> TfAssign -> TfAssign
-- if some assign is null, return the other one
-- in practice, the argument cannot be null, as it is already filtered
-- in toTf function
comb2Assigns TfAssignNull a = a
comb2Assigns a TfAssignNull = a
-- the result cannot be null, as the null case has been handled
comb2Assigns a1 a2 = TfAssign $ foldr updateOrAppend [] assignList
  where
    -- concat two assign lists into one,
    -- each element is an TfAssignItem
    -- the second is appended to the first one
    assignList =
      let TfAssign l1 = a1
          TfAssign l2 = a2
      -- reverse the order because of foldr starts from the end
       in l2 ++ l1
    -- if the variable is already assigned in the list, delete it first
    -- append the new assign to the list
    updateOrAppend :: TfAssignItem -> [TfAssignItem] -> [TfAssignItem]
    updateOrAppend (TfAssignItem v e) as = as' ++ [TfAssignItem v e]
      where
        as' = filter (\(TfAssignItem v' _) -> v /= v') as

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
