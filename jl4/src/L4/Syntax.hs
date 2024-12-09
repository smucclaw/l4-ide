module L4.Syntax where

import Data.Text (Text)
import Data.TreeDiff (ToExpr)
import qualified GHC.Generics as GHC

type Name = Text
type Label = Name

data Type' n =
    NamedType n
  | Enum      [n]
  | Record    [TypedName n]
  | Boolean -- should perhaps just be a pre-defined NamedType
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data TypedName n =
  MkTypedName n (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data TypeSig n =
  MkTypeSig [TypedName n] (Maybe (TypedName n))
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data Decide n =
  MkDecide (TypeSig n) [Clause n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data Declare n =
  MkDeclare n (Type' n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data Clause n =
  GuardedClause (Expr n) (Guard n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data Guard n =
    PlainGuard (Expr n)
  | Otherwise
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data Expr n =
    And  (Expr n) (Expr n)
  | Or   (Expr n) (Expr n)
  | Is   (Expr n) (Expr n)
  | Not  (Expr n)
  | Proj (Expr n) Label
  | Var  n
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data Program n =
  MkProgram [Section n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

data Section n =
  MkSection SectionLevel n [Decl n]
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

type SectionLevel = Int

data Decl n =
    Declare (Declare n)
  | Decide  (Decide n)
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass ToExpr

pitchDecks :: Program Name
pitchDecks =
  MkProgram
    [ MkSection 1 "types"
        [ Declare $ MkDeclare "Business"
            (Record
              [ MkTypedName "stage"     (Enum ["Pre_Seed", "Seed", "Series A", "Series B", "other stage"])
              , MkTypedName "stage_com" (Enum ["Pre_Revenue", "Pre_Profit", "Profit", "other stage_com"])
              , MkTypedName "sector"    (Enum ["Energy", "Healthcare", "Real Estate", "Information Technology", "Financials", "Consumer Discretionary", "Communication", "Industrials", "Consumer Stables", "Energy", "Utilities", "Real Estate", "Materials", "other_sector"])
              ]
            )
        , Declare $ MkDeclare "Investor"
            (Record
              [ MkTypedName "wants_ESG" Boolean
              ]
            )
        , Declare $ MkDeclare "Opinion"
            (Enum ["interesting", "reject", "shrug"])
        ]
    , MkSection 1 "interesting cases"
        [ Decide $ MkDecide
            (MkTypeSig
              [ MkTypedName "b"   (NamedType "Business")
              , MkTypedName "inv" (NamedType "Investor")
              ]
              (Just (MkTypedName "opinion" (NamedType "Opinion")))
            )
            [ GuardedClause (Is (Var "opinion") (Var "interesting")) (PlainGuard (Var "undefined"))
            , GuardedClause (Is (Var "opinion") (Var "reject")) Otherwise
            ]
        ]
    ]

parsed :: Program Name
parsed =
  MkProgram [MkSection 1 "types" [Declare (MkDeclare "Business" (Record [MkTypedName "stage" (Enum ["Pre_Seed","Seed","Series A","Series B","other stage"]),MkTypedName "stage_com" (Enum ["Pre_Revenue","Pre_Proft","Profit","other stage_com"]),MkTypedName "sector" (Enum ["Energy","Healthcare","Real Estate","Information Technology","Healthcare","Financials","Consumer Discretionary","Communication","Industrials","Consumer Staples","Energy","Utilities","Real Estate","Materials","other_sector"])])),Declare (MkDeclare "Investor" (Record [MkTypedName "wants_ESG" (NamedType "Boolean")])),Declare (MkDeclare "Opinion" (Enum ["interesting","reject","shrug"]))],MkSection 1 "interesting cases" [Decide (MkDecide (MkTypeSig [MkTypedName "b" (NamedType "Business"),MkTypedName "inv" (NamedType "Investor")] (Just (MkTypedName "opinion" (NamedType "Opinion")))) [GuardedClause (Is (Var "opinion") (Var "interesting")) (PlainGuard (Or (And (Is (Proj (Var "b") "stage") (Var "Seed")) (And (Is (Proj (Var "b") "sector") (Var "Information Technology")) (Is (Proj (Var "b") "stage_com") (Var "Pre_Revenue")))) (Or (And (Is (Proj (Var "b") "stage") (Var "Series A")) (And (Is (Proj (Var "b") "sector") (Var "Information Technology")) (Is (Proj (Var "b") "stage_com") (Var "Pre_Profit")))) (And (Proj (Var "inv") "wants_ESG") (Proj (Var "b") "has_ESG"))))),GuardedClause (Is (Var "opinion") (Var "reject")) Otherwise])]]
