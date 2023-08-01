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

-- simplify one TfCondition
simplifyCond :: TfCondition -> TfCondition
simplifyCond cond =
  case cond of
    TfAnd TfTrue c1 -> simplifyCond c1
    TfAnd c1 TfTrue -> simplifyCond c1
    TfAnd TfFalse _ -> TfFalse
    TfAnd _ TfFalse -> TfFalse
    -- TfAnd c1 c2 -> TfAnd (simplifyCond c1) (simplifyCond c2)
    -- if one of the simplified condition if TfFalse or TfTrue, then they remain
    -- so need to simplify again based on the simplified condition
    TfAnd c1 c2 ->
      let c1' = simplifyCond c1
          c2' = simplifyCond c2
       in simplifyCond $ TfAnd c1' c2'
    TfOr TfTrue _ -> TfTrue
    TfOr _ TfTrue -> TfTrue
    TfOr TfFalse c1 -> simplifyCond c1
    TfOr c1 TfFalse -> simplifyCond c1
    -- TfOr c1 c2 -> TfOr (simplifyCond c1) (simplifyCond c2)
    TfOr c1 c2 ->
      let c1' = simplifyCond c1
          c2' = simplifyCond c2
       in simplifyCond $ TfOr c1' c2'
    TfNot TfTrue -> TfFalse
    TfNot TfFalse -> TfTrue
    TfNot (TfNot c1) -> simplifyCond c1
    TfNot (TfCond e1 op e2) -> simplifyCond $ TfCond e1 (reverseOp op) e2
    -- TfNot (TfAnd c1 c2) ->
    --   TfOr (simplifyCond $ TfNot c1) (simplifyCond $ TfNot c2)
    TfNot (TfAnd c1 c2) ->
      let c1' = simplifyCond $ TfNot c1
          c2' = simplifyCond $ TfNot c2
       in simplifyCond $ TfOr c1' c2'
    -- TfNot (TfOr c1 c2) ->
    --   TfAnd (simplifyCond $ TfNot c1) (simplifyCond $ TfNot c2)
    TfNot (TfOr c1 c2) ->
      let c1' = simplifyCond $ TfNot c1
          c2' = simplifyCond $ TfNot c2
       in simplifyCond $ TfAnd c1' c2'
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
