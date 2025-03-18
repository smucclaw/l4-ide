-- Ad-hoc logical transformations of resolved expressions.
--
-- Ideally, these should take more type information into account.
--
module L4.Transform where

import L4.Annotation
import L4.Syntax

simplify :: Expr Resolved -> Expr Resolved
simplify = cnf . nnf

nnf :: Expr Resolved -> Expr Resolved
nnf (Not _ e)         = neg (nnf e)
nnf (And _ e1 e2)     = And emptyAnno (nnf e1) (nnf e2)
nnf (Or _ e1 e2)      = Or emptyAnno (nnf e1) (nnf e2)
nnf (Implies _ e1 e2) = nnf (implies e1 e2)
nnf (Where _ e _ds)   = nnf e -- TODO: lossy
nnf e                 = e

cnf :: Expr Resolved -> Expr Resolved
cnf (And _ (And _ e1 e2) e3) = cnf (And emptyAnno e1 (And emptyAnno e2 e3))
cnf (And _ e1 e2)            = And emptyAnno (cnf e1) (cnf e2)
cnf (Or _ (Or _ e1 e2) e3)   = cnf (Or emptyAnno e1 (Or emptyAnno e2 e3))
cnf (Or _ e1 e2)             = distr (cnf e1) (cnf e2)
cnf (Implies _ e1 e2)        = cnf (implies e1 e2)
cnf (Where _ e _ds)          = cnf e -- TODO: lossy
cnf e                        = e

-- | Try to exploit distributivity laws.
distr :: Expr Resolved -> Expr Resolved -> Expr Resolved
distr (And _ e1 e2) e3 = And emptyAnno (distr e1 e3) (distr e2 e3)
distr e1 (And _ e2 e3) = And emptyAnno (distr e1 e2) (distr e1 e3)
distr e1 e2            = Or emptyAnno e1 e2

-- | Try to push down a negation.
neg :: Expr Resolved -> Expr Resolved
neg (Not _ e)         = e
neg (And _ e1 e2)     = Or emptyAnno (neg e1) (neg e2)
neg (Or _ e1 e2)      = And emptyAnno (neg e1) (neg e2)
neg (Implies _ e1 e2) = neg (implies e1 e2)
-- the following would be great, but we'd have to check the types
-- neg (Equals _ e1 e2)  = equivExpr e1 e2
-- neg (IfThenElse _ e1 e2 e3) = ...
neg (Where _ e _ds)   = neg e -- TODO: lossy
neg e                 = Not emptyAnno e

-- | Classically interpret implication.
implies :: Expr Resolved -> Expr Resolved -> Expr Resolved
implies e1 e2 = Or emptyAnno (Not emptyAnno e1) e2

-- | Classically interpret equivalence.
equiv :: Expr Resolved -> Expr Resolved -> Expr Resolved
equiv e1 e2 = And emptyAnno (implies e1 e2) (implies e2 e1)


