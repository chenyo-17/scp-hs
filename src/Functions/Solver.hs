{-# LANGUAGE GADTs #-}

module Functions.Solver where

import           Data.SBV
import           Data.SBV.Internals
import           Functions.Transfer
import           System.IO.Unsafe   (unsafePerformIO)

data ExprWrapper where
  EBool :: SBool -> ExprWrapper
  EInt :: SInteger -> ExprWrapper
  EString :: SString -> ExprWrapper

toSBVLiteral :: TfLiteral -> ExprWrapper
toSBVLiteral (TfInt i)  = EInt $ literal (toInteger i)
toSBVLiteral (TfBool b) = EBool $ literal b

toSBVExpr :: TfExpr -> Symbolic ExprWrapper
-- TODO: consider other variable types
toSBVExpr (TfVar s)     = EInt <$> sInteger s
toSBVExpr (TfConst lit) = return $ toSBVLiteral lit
toSBVExpr TfNull        = EInt <$> sInteger "null"

applyOp :: TfOp -> ExprWrapper -> ExprWrapper -> SBool
applyOp op (EInt e1) (EInt e2) =
  case op of
    TfGe -> e1 .>= e2
    TfLe -> e1 .<= e2
    TfGt -> e1 .> e2
    TfLt -> e1 .< e2
    TfEq -> e1 .== e2
    TfNe -> e1 ./= e2
applyOp op (EBool e1) (EBool e2) =
  case op of
    TfEq -> e1 .== e2
    TfNe -> e1 ./= e2
    _    -> sFalse
applyOp op (EString e1) (EString e2) =
  case op of
    TfEq -> e1 .== e2
    TfNe -> e1 ./= e2
    _    -> sFalse
applyOp _ _ _ = sFalse

toSBVCond :: TfCondition -> Symbolic SBool
toSBVCond TfTrue = return sTrue
toSBVCond TfFalse = return sFalse
toSBVCond (TfNot c) = sNot <$> toSBVCond c
toSBVCond (TfAnd c1 c2) = (.&&) <$> toSBVCond c1 <*> toSBVCond c2
toSBVCond (TfOr c1 c2) = (.||) <$> toSBVCond c1 <*> toSBVCond c2
toSBVCond (TfCond e1 op e2) = do
  se1 <- toSBVExpr e1
  se2 <- toSBVExpr e2
  return $ applyOp op se1 se2

-- FIXME: this is a hack!
checkSat :: TfCondition -> Bool
checkSat cond =
  unsafePerformIO $ do
    satRes <- sat $ toSBVCond cond
    case satRes of
      SatResult (Satisfiable _ _) -> return True
      _                           -> return False

simplifyCondWithSolver :: TfCondition -> (TfCondition, Maybe SMTModel)
simplifyCondWithSolver cond =
  unsafePerformIO $ do
    satRes <- sat $ toSBVCond cond
    case satRes of
      SatResult (Satisfiable _ model) -> return (cond, Just model)
      _                               -> return (TfFalse, Nothing)
