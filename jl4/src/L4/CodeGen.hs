module L4.CodeGen where

import qualified Simala.Expr.Type as Simala
import L4.Syntax

translateDecide :: Decide Name -> Simala.Expr
translateDecide (MkDecide _ tsig clauses) =
  translateTypeSig tsig (translateClauses clauses)

translateTypeSig :: TypeSig Name -> Simala.Expr -> Simala.Expr
translateTypeSig (MkTypeSig _ (MkGivenSig _ tns) _) e =
  Simala.Fun Simala.Transparent (extractNames tns) e

extractNames :: [TypedName Name] -> [Simala.Name]
extractNames = map (\ (MkTypedName _ (Name _ n) _) -> n)

translateClauses :: [Clause Name] -> Simala.Expr
translateClauses [] = Simala.Undefined
translateClauses (GuardedClause _ e (Otherwise _) : _) = translateExpr e
translateClauses (GuardedClause _ e (PlainGuard _ c) : clauses) =
  Simala.Builtin Simala.IfThenElse [translateExpr c, translateExpr e, translateClauses clauses]

translateExpr :: Expr Name -> Simala.Expr
translateExpr (And _ e1 e2) = Simala.Builtin Simala.And [translateExpr e1, translateExpr e2]
translateExpr (Or _ e1 e2)  = Simala.Builtin Simala.Or  [translateExpr e1, translateExpr e2]
translateExpr (Is _ e1 e2)  = Simala.Builtin Simala.Eq  [translateExpr e1, translateExpr e2]
translateExpr (Not _ e)     = Simala.Builtin Simala.Not [translateExpr e]
translateExpr (Proj _ e (Name _ l))  = Simala.Project (translateExpr e) l
translateExpr (Var _ (Name _ n))     = Simala.Var n


