{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LSP.L4.SemanticTokens where

import Base

import LSP.SemanticTokens
import Language.LSP.Protocol.Types hiding (Pattern)

import L4.Lexer
import L4.Parser.SrcSpan
import L4.Syntax
import Optics

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
standardTokenType = \ case
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
instance ToSemTokens Context PosToken (TopDecl Name) where

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

instance ToSemTokens Context PosToken (Event Name) where
  toSemTokens (MkEvent ann p a t atFirst) = traverseCsnWithHoles ann $ map toSemTokens
    if atFirst then [p, a, t] else [t, p, a]



instance ToSemTokens Context PosToken (Expr Name) where
instance ToSemTokens Context PosToken (Obligation Name) where
instance ToSemTokens Context PosToken (RAction Name) where
instance ToSemTokens Context PosToken (LocalDecl Name) where
instance ToSemTokens Context PosToken (Assume Name) where
instance ToSemTokens Context PosToken (TypedName Name) where
instance ToSemTokens Context PosToken (OptionallyTypedName Name) where
instance ToSemTokens Context PosToken (OptionallyNamedType Name) where
instance ToSemTokens Context PosToken (Decide Name) where
instance ToSemTokens Context PosToken (Aka Name) where
instance ToSemTokens Context PosToken (TypeDecl Name) where
instance ToSemTokens Context PosToken (NamedExpr Name) where
instance ToSemTokens Context PosToken (Branch Name) where
  toSemTokens = genericToSemTokens
instance ToSemTokens Context PosToken (Pattern Name) where
  toSemTokens = genericToSemTokens
instance ToSemTokens Context PosToken (TypeSig Name) where
instance ToSemTokens Context PosToken (GivethSig Name) where
instance ToSemTokens Context PosToken (GivenSig Name) where
instance ToSemTokens Context PosToken (Directive Name) where
instance ToSemTokens Context PosToken (Import Name) where

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

instance ToSemTokens () PosToken Resolved where
  toSemTokens r = do
    let n = getActual r in modifier (infoOf n) $ toSemTokens n
    where
      infoOf n = n ^. annoOf % annInfo
      modifier = \ case
        Nothing -> id
        Just info -> case info of
          KindInfo _kind -> withTokenType identIsType
          TypeInfo ty termKind ->
              case termKind of
                Just Computable -> highlightFunction ty
                Just Assumed -> withTokenType identIsAssume
                Just Local -> highlightFunction ty
                Just Constructor -> withTokenType identIsCon
                Just Selector -> withTokenType identIsSelector
                Nothing -> id
          TypeVariable ->
            withTokenType identIsTypeVar

      highlightFunction = \case
        Fun{} -> withTokenType identIsFunction
        Forall _ _ ty -> highlightFunction ty
        _ -> id

instance ToSemTokens () PosToken (Module  Resolved) where
  toSemTokens (MkModule ann _uri sects) =
    traverseCsnWithHoles ann [toSemTokens sects]
instance ToSemTokens () PosToken (TopDecl Resolved) where

instance ToSemTokens () PosToken (Section Resolved) where
  toSemTokens (MkSection ann name maka decls) =
    traverseCsnWithHoles ann
      [ withTokenType identIsDirective $ toSemTokens name
      , withTokenType identIsDirective $ toSemTokens maka
      , toSemTokens decls
      ]
instance ToSemTokens () PosToken (Declare Resolved) where
instance ToSemTokens () PosToken (ConDecl Resolved) where
instance ToSemTokens () PosToken (Type' Resolved) where

instance ToSemTokens () PosToken (AppForm Resolved) where

instance ToSemTokens () PosToken (Obligation Resolved) where
instance ToSemTokens () PosToken (RAction Resolved) where
instance ToSemTokens () PosToken (Event Resolved) where
  toSemTokens (MkEvent ann p a t atFirst) = traverseCsnWithHoles ann $ map toSemTokens
    if atFirst then [t, p, a] else [p, a, t]
instance ToSemTokens () PosToken (Expr Resolved) where
instance ToSemTokens () PosToken (LocalDecl Resolved) where
instance ToSemTokens () PosToken (Assume Resolved) where
instance ToSemTokens () PosToken (TypedName Resolved) where
instance ToSemTokens () PosToken (OptionallyTypedName Resolved) where
instance ToSemTokens () PosToken (OptionallyNamedType Resolved) where
instance ToSemTokens () PosToken (Decide Resolved) where
instance ToSemTokens () PosToken (Aka Resolved) where
instance ToSemTokens () PosToken (TypeDecl Resolved) where
instance ToSemTokens () PosToken (NamedExpr Resolved) where
instance ToSemTokens () PosToken (Branch Resolved) where
instance ToSemTokens () PosToken (Pattern Resolved) where
instance ToSemTokens () PosToken (TypeSig Resolved) where
instance ToSemTokens () PosToken (GivethSig Resolved) where
instance ToSemTokens () PosToken (GivenSig Resolved) where
instance ToSemTokens () PosToken (Directive Resolved) where
instance ToSemTokens () PosToken (Import Resolved) where
instance ToSemTokens () PosToken NormalizedUri where
  toSemTokens _ = pure []

instance ToSemTokens () PosToken Int where
  toSemTokens _ = pure []

instance ToSemTokens () PosToken Name where
  toSemTokens (MkName ann _) =
    traverseCsnWithHoles ann []

instance ToSemTokens () PosToken RawName where
  toSemTokens _ = pure []

instance ToSemTokens () PosToken Lit where
  toSemTokens (NumericLit ann _) =
    traverseCsnWithHoles ann []
  toSemTokens (StringLit ann _) =
    traverseCsnWithHoles ann []
