{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LSP.L4.SemanticTokens where

import Base
import qualified Data.Map.Strict as Map

import LSP.SemanticTokens
import Language.LSP.Protocol.Types hiding (Pattern)

import L4.Lexer
import L4.Parser.SrcSpan
import L4.Syntax
import L4.TypeCheck.Types

-- ----------------------------------------------------------------------------
-- JL4 specific implementation
-- ----------------------------------------------------------------------------

defaultSemanticTokenCtx :: c -> SemanticTokenCtx c PosToken
defaultSemanticTokenCtx c =
  SemanticTokenCtx
    { semanticTokenType = simpleTokenType
    , semanticTokenModifier = \_ -> pure []
    , semanticTokenContext = c
    }

standardTokenType :: TokenCategory -> Maybe SemanticTokenTypes
standardTokenType = \case
  CIdentifier -> Just SemanticTokenTypes_Variable
  CStringLit -> Just SemanticTokenTypes_String
  CNumberLit -> Just SemanticTokenTypes_Number
  CSymbol -> Just SemanticTokenTypes_Operator
  COperator -> Just SemanticTokenTypes_Operator
  CKeyword -> Just SemanticTokenTypes_Keyword
  CComment -> Just SemanticTokenTypes_Comment
  CWhitespace -> Nothing
  CDirective -> Just SemanticTokenTypes_Macro
  CAnnotation -> Just SemanticTokenTypes_Decorator
  CEOF -> Nothing

simpleTokenType :: PosToken -> Maybe SemanticTokenTypes
simpleTokenType t = standardTokenType (posTokenCategory t.payload)

identIsParameter :: PosToken -> Maybe SemanticTokenTypes
identIsParameter t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Parameter
  _ -> Nothing

identIsDirective :: PosToken -> Maybe SemanticTokenTypes
identIsDirective t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Interface
  _ -> Nothing

enumType :: PosToken -> Maybe SemanticTokenTypes
enumType t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Enum
  _ -> Nothing

defVar :: PosToken -> Maybe [SemanticTokenModifiers]
defVar t = case posTokenCategory t.payload of
  CIdentifier -> Just [SemanticTokenModifiers_Declaration, SemanticTokenModifiers_Definition]
  _ -> Nothing

identIsType :: PosToken -> Maybe SemanticTokenTypes
identIsType t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Type
  _ -> Nothing

identIsTypeVar :: PosToken -> Maybe SemanticTokenTypes
identIsTypeVar t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_TypeParameter
  _ -> Nothing

identIsCon :: PosToken -> Maybe SemanticTokenTypes
identIsCon t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Class
  _ -> Nothing

identIsFunction :: PosToken -> Maybe SemanticTokenTypes
identIsFunction t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Function
  _ -> Nothing

identIsSelector :: PosToken -> Maybe SemanticTokenTypes
identIsSelector t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Method
  _ -> Nothing

identIsAssume :: PosToken -> Maybe SemanticTokenTypes
identIsAssume t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Event
  _ -> Nothing

identIsSynonym :: PosToken -> Maybe SemanticTokenTypes
identIsSynonym t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Type
  _ -> Nothing

srcPosToPosition :: SrcPos -> Position
srcPosToPosition s =
  Position
    { _character = fromIntegral s.column - 1
    , _line = fromIntegral s.line - 1
    }

withTypeContext :: SemanticTokensM Context t a -> SemanticTokensM Context t a
withTypeContext =
  withContext CType

withTypeSigContext :: SemanticTokensM Context t a -> SemanticTokensM Context t a
withTypeSigContext =
  withContext CTypeSig

withValueContext :: SemanticTokensM Context t a -> SemanticTokensM Context t a
withValueContext =
  withContext CValue

data Context
  = CValue
  | CType
  | CTypeSig
  deriving (Show, Eq, Ord)

-- ----------------------------------------------------------------------------
-- Simala AST to Semantic Tokens
-- ----------------------------------------------------------------------------

instance ToSemToken PosToken where
  toSemToken :: PosToken -> SemanticTokenTypes -> [SemanticTokenModifiers] -> NonEmpty SemanticToken
  toSemToken token category modifiers
    | token.range.start.line /= token.range.end.line =
        splitTokens (mkSingleSemToken token) (displayPosToken token)
    | otherwise =
        pure (mkSingleSemToken token)
   where
    mkSingleSemToken :: PosToken -> SemanticToken
    mkSingleSemToken t = SemanticToken
      { start = srcPosToPosition t.range.start
      , length = fromIntegral t.range.length
      , category = category
      , modifiers = modifiers
      }

-- ----------------------------------------------------------------------------
-- Semantic Tokens for Parsed AST
-- ----------------------------------------------------------------------------

instance ToSemTokens Context PosToken (Module  Name) where
  toSemTokens (MkModule ann _uri sects) =
    traverseCsnWithHoles ann [toSemTokens sects]
deriving anyclass instance ToSemTokens Context PosToken (TopDecl Name)

instance ToSemTokens Context PosToken (Section Name) where
  toSemTokens (MkSection ann name maka decls) =
    traverseCsnWithHoles ann
      [ withTokenType identIsDirective $ toSemTokens name
      , withTokenType identIsDirective $ toSemTokens maka
      , toSemTokens decls
      ]

instance ToSemTokens Context PosToken (Declare Name) where
  toSemTokens (MkDeclare ann typesig appformAka decl) =
    traverseCsnWithHoles ann
      [ withTokenType identIsType $ toSemTokens typesig
      , withTypeSigContext $ toSemTokens appformAka
      , toSemTokens decl
      ]

instance ToSemTokens Context PosToken (ConDecl Name) where
  toSemTokens (MkConDecl ann n ns) =
    traverseCsnWithHoles ann
      [ withTokenType identIsCon $ toSemTokens n
      , toSemTokens ns
      ]

instance ToSemTokens Context PosToken (Type' Name) where
  toSemTokens ty = withTokenType identIsType $ genericToSemTokens ty

instance ToSemTokens Context PosToken (AppForm Name) where
  toSemTokens a@(MkAppForm ann n ns maka) = do
    c <- asks (.semanticTokenContext)
    case c of
      CTypeSig ->
        traverseCsnWithHoles ann
          [ withTokenType identIsType $ toSemTokens n
          , withTokenType identIsTypeVar $ toSemTokens ns
          , withTokenType identIsType $ toSemTokens maka
          ]
      CType -> genericToSemTokens a
      CValue -> genericToSemTokens a

deriving anyclass instance ToSemTokens Context PosToken (Expr Name)
deriving anyclass instance ToSemTokens Context PosToken (LocalDecl Name)
deriving anyclass instance ToSemTokens Context PosToken (Assume Name)
deriving anyclass instance ToSemTokens Context PosToken (TypedName Name)
deriving anyclass instance ToSemTokens Context PosToken (OptionallyTypedName Name)
deriving anyclass instance ToSemTokens Context PosToken (OptionallyNamedType Name)
deriving anyclass instance ToSemTokens Context PosToken (Decide Name)
deriving anyclass instance ToSemTokens Context PosToken (Aka Name)
deriving anyclass instance ToSemTokens Context PosToken (TypeDecl Name)
deriving anyclass instance ToSemTokens Context PosToken (NamedExpr Name)
instance ToSemTokens Context PosToken (Branch Name) where
  toSemTokens = genericToSemTokens
instance ToSemTokens Context PosToken (Pattern Name) where
  toSemTokens = genericToSemTokens
deriving anyclass instance ToSemTokens Context PosToken (TypeSig Name)
deriving anyclass instance ToSemTokens Context PosToken (GivethSig Name)
deriving anyclass instance ToSemTokens Context PosToken (GivenSig Name)
deriving anyclass instance ToSemTokens Context PosToken (Directive Name)
deriving anyclass instance ToSemTokens Context PosToken (Import Name)

instance ToSemTokens Context PosToken NormalizedUri where
  toSemTokens _ = pure []

instance ToSemTokens Context PosToken Int where
  toSemTokens _ = pure []

instance ToSemTokens Context PosToken Name where
  toSemTokens (MkName ann _) =
    traverseCsnWithHoles ann []

instance ToSemTokens Context PosToken RawName where
  toSemTokens _ = pure []

instance ToSemTokens Context PosToken Lit where
  toSemTokens (NumericLit ann _) =
    traverseCsnWithHoles ann []
  toSemTokens (StringLit ann _) =
    traverseCsnWithHoles ann []

instance ToSemTokens () PosToken PosToken where
  toSemTokens t = do
    ctx <- ask
    pure $ maybe [] toList (fromSemanticTokenContext ctx t)

--- ---------------------------------------------------------------------------
-- Semantic Tokens for typechecked AST
--- ---------------------------------------------------------------------------

instance ToSemTokens EntityInfo PosToken Resolved where
  toSemTokens r = do
    entityInfo <- asks (.semanticTokenContext)
    case r of
      Def _ n -> modifier entityInfo $ withModifier defVar $ toSemTokens n
      Ref orig _ _ -> modifier entityInfo $ toSemTokens orig
      OutOfScope _ n -> modifier entityInfo $ toSemTokens n
    where
      uniq = getUnique r
      modifier entityInfo =
        case Map.lookup uniq entityInfo of
          -- Strictly speaking, this is an internal invariant violation.
          -- However, we likely shouldn't care during syntax highlighting.
          Nothing -> id
          Just (_, entity) -> case entity of
            KnownType _kind _names synonym -> case synonym of
              Nothing -> withTokenType identIsType
              Just _ty -> withTokenType identIsSynonym
            KnownTerm ty termKind ->
                case termKind of
                  Computable -> basedOnType ty
                  Assumed ->
                    withTokenType identIsAssume
                  Local -> basedOnType ty
                  Constructor -> withTokenType identIsCon
                  Selector -> withTokenType identIsSelector
            KnownSection _section -> withTokenType identIsDirective
            KnownTypeVariable ->
              withTokenType identIsTypeVar

      basedOnType = \case
        Type{} -> withTokenType identIsType
        Fun{} -> withTokenType identIsFunction
        TyApp{} -> withTokenType identIsType
        Forall _ _ ty -> basedOnType ty
        InfVar{} -> id

instance ToSemTokens EntityInfo PosToken (Module  Resolved) where
  toSemTokens (MkModule ann _uri sects) =
    traverseCsnWithHoles ann [toSemTokens sects]
deriving anyclass instance ToSemTokens EntityInfo PosToken (TopDecl Resolved)

instance ToSemTokens EntityInfo PosToken (Section Resolved) where
  toSemTokens (MkSection ann name maka decls) =
    traverseCsnWithHoles ann
      [ withTokenType identIsDirective $ toSemTokens name
      , withTokenType identIsDirective $ toSemTokens maka
      , toSemTokens decls
      ]
deriving anyclass instance ToSemTokens EntityInfo PosToken (Declare Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (ConDecl Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (Type' Resolved)

deriving anyclass instance ToSemTokens EntityInfo PosToken (AppForm Resolved)

deriving anyclass instance ToSemTokens EntityInfo PosToken (Expr Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (LocalDecl Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (Assume Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (TypedName Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (OptionallyTypedName Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (OptionallyNamedType Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (Decide Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (Aka Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (TypeDecl Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (NamedExpr Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (Branch Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (Pattern Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (TypeSig Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (GivethSig Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (GivenSig Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (Directive Resolved)
deriving anyclass instance ToSemTokens EntityInfo PosToken (Import Resolved)
instance ToSemTokens EntityInfo PosToken NormalizedUri where
  toSemTokens _ = pure []

instance ToSemTokens EntityInfo PosToken Int where
  toSemTokens _ = pure []

instance ToSemTokens EntityInfo PosToken Name where
  toSemTokens (MkName ann _) =
    traverseCsnWithHoles ann []

instance ToSemTokens EntityInfo PosToken RawName where
  toSemTokens _ = pure []

instance ToSemTokens EntityInfo PosToken Lit where
  toSemTokens (NumericLit ann _) =
    traverseCsnWithHoles ann []
  toSemTokens (StringLit ann _) =
    traverseCsnWithHoles ann []
