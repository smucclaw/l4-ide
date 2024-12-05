module L4.CodeGen where

import qualified Simala.Expr.Type as Simala
import L4.Syntax

translateDecide :: Decide Name -> Simala.Expr
translateDecide (MkDecide tsig clauses) =
  translateTypeSig tsig (translateClauses clauses)

translateTypeSig :: TypeSig Name -> Simala.Expr -> Simala.Expr
translateTypeSig (MkTypeSig tns _) e =
  Simala.Fun Simala.Transparent (extractNames tns) e

extractNames :: [TypedName Name] -> [Name]
extractNames = map (\ (MkTypedName n _) -> n)

translateClauses :: [Clause Name] -> Simala.Expr
translateClauses [] = Simala.Undefined
translateClauses (GuardedClause e Otherwise : _) = translateExpr e
translateClauses (GuardedClause e (PlainGuard c) : clauses) =
  Simala.Builtin Simala.IfThenElse [translateExpr c, translateExpr e, translateClauses clauses]

translateExpr :: Expr Name -> Simala.Expr
translateExpr (And e1 e2) = Simala.Builtin Simala.And [translateExpr e1, translateExpr e2]
translateExpr (Or e1 e2)  = Simala.Builtin Simala.Or  [translateExpr e1, translateExpr e2]
translateExpr (Is e1 e2)  = Simala.Builtin Simala.Eq  [translateExpr e1, translateExpr e2]
translateExpr (Not e)     = Simala.Builtin Simala.Not [translateExpr e]
translateExpr (Proj e l)  = Simala.Project (translateExpr e) l
translateExpr (Var n)     = Simala.Var n


