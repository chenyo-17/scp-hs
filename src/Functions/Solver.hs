module Functions.Solver
  ( simplifyCondWithSolver
  ) where

import qualified Data.Map           as Map
import           Data.SBV
import           Functions.Transfer
import           GHC.IO

data SExpr
  = EBool SBool
  | EInt SInt32

type SymDict = Map.Map String SInt32

toSBVLiteral :: TfLiteral -> SExpr
toSBVLiteral (TfInt i)  = EInt $ literal (fromIntegral i :: Int32)
toSBVLiteral (TfBool b) = EBool $ literal b
-- assume there is only positive integer constraints
toSBVLiteral TfNull     = EInt (-1)

-- keep the state of symbolic variables
toSBVExpr :: SymDict -> TfExpr -> Symbolic (SymDict, SExpr)
toSBVExpr dict (TfVar s) =
  case Map.lookup s dict of
    Just symVar -> return (dict, EInt symVar)
    Nothing -> do
      newVar <- sInt32 s
      let newDict = Map.insert s newVar dict
      return (newDict, EInt newVar)
toSBVExpr dict (TfConst lit) = return (dict, toSBVLiteral lit)
toSBVExpr dict (TfAdd e1 e2) = do
  (newDict, EInt se1) <- toSBVExpr dict e1
  (newDict', EInt se2) <- toSBVExpr newDict e2
  return (newDict', EInt $ se1 + se2)

applyOp :: TfOpA -> SExpr -> SExpr -> SBool
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
applyOp _ _ _ = sFalse

toSBVCond :: TfCondition -> Symbolic SBool
toSBVCond cond = snd <$> toSBVCond' Map.empty cond
  where
    toSBVCond' :: SymDict -> TfCondition -> Symbolic (SymDict, SBool)
    toSBVCond' dict TfTrue = return (dict, sTrue)
    toSBVCond' dict TfFalse = return (dict, sFalse)
    toSBVCond' dict (TfNot c) = do
      (newDict, sc) <- toSBVCond' dict c
      return (newDict, sNot sc)
    toSBVCond' dict (TfAnd c1 c2) = do
      (newDict, sc1) <- toSBVCond' dict c1
      (newDict', sc2) <- toSBVCond' newDict c2
      return (newDict', sc1 .&& sc2)
    toSBVCond' dict (TfOr c1 c2) = do
      (newDict, sc1) <- toSBVCond' dict c1
      (newDict', sc2) <- toSBVCond' newDict c2
      return (newDict', sc1 .|| sc2)
    toSBVCond' dict (TfCond e1 op e2) = do
      (newDict, se1) <- toSBVExpr dict e1
      (newDict', se2) <- toSBVExpr newDict e2
      return (newDict', applyOp op se1 se2)

simplifyCondWithSolver :: TfCondition -> TfCondition
simplifyCondWithSolver cond =
  unsafePerformIO $ do
    satRes <- sat $ toSBVCond cond
    case satRes of
      SatResult (Satisfiable _ _) -> return cond
      _                           -> return TfFalse
