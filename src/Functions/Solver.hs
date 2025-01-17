module Functions.Solver
  ( simplifyCondWithSolver
  ) where

import qualified Data.Map           as Map
import           Data.SBV
import           Functions.Transfer

data SExpr
  = EBool SBool
  | EWord SWord32
  | EInt SInt32

type SymDict = Map.Map String SWord32

toSBVLiteral :: TfLiteral -> SExpr
toSBVLiteral (TfInt i)  = EWord $ literal (fromIntegral i :: Word32)
toSBVLiteral (TfBool b) = EBool $ literal b
-- assume there is only positive integer constraints
toSBVLiteral TfNull     = EInt (-1)

-- keep the state of symbolic variables
toSBVExpr :: SymDict -> TfExpr -> Symbolic (SymDict, SExpr)
toSBVExpr dict (TfVar s) =
  case Map.lookup s dict of
    Just symVar -> return (dict, EWord symVar)
    Nothing -> do
      newVar <- sWord32 s
      let newDict = Map.insert s newVar dict
      return (newDict, EWord newVar)
toSBVExpr dict (TfConst lit) = return (dict, toSBVLiteral lit)
toSBVExpr dict (TfAdd e1 e2) = do
  (newDict, EWord se1) <- toSBVExpr dict e1
  (newDict', EWord se2) <- toSBVExpr newDict e2
  return (newDict', EWord $ se1 + se2)

applyOp :: TfOpA -> SExpr -> SExpr -> SBool
applyOp op (EWord e1) (EWord e2) =
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
    toSBVCond' dict (TfImply c1 c2) = do
      (newDict, sc1) <- toSBVCond' dict c1
      (newDict', sc2) <- toSBVCond' newDict c2
      return (newDict', sNot sc1 .|| (sc1 .&& sc2))

simplifyCondWithSolver :: TfCondition -> IO TfCondition
simplifyCondWithSolver TfFalse = return TfFalse
simplifyCondWithSolver TfTrue  = return TfTrue
simplifyCondWithSolver cond = do
  satRes <- sat $ toSBVCond cond
  case satRes of
    SatResult (Satisfiable _ _) -> return cond
    _                           -> return TfFalse
