{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LSP.L4.SemanticTokens where

import Base
import LSP.L4.Base

import LSP.SemanticTokens
import Language.LSP.Protocol.Types hiding (Pattern)

-- ----------------------------------------------------------------------------
-- JL4 specific implementation
-- ----------------------------------------------------------------------------

defaultSemanticTokenCtx :: SemanticTokenCtx PosToken
defaultSemanticTokenCtx =
  SemanticTokenCtx
    { semanticTokenType = simpleTokenType
    , semanticTokenModifier = \_ -> pure []
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

parameterType :: PosToken -> Maybe SemanticTokenTypes
parameterType t = case posTokenCategory t.payload of
  CIdentifier -> Just SemanticTokenTypes_Parameter
  _ -> Nothing

nameIsDirective :: PosToken -> Maybe SemanticTokenTypes
nameIsDirective t = case posTokenCategory t.payload of
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

srcPosToPosition :: SrcPos -> Position
srcPosToPosition s =
  Position
    { _character = fromIntegral s.column - 1
    , _line = fromIntegral s.line - 1
    }

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

deriving anyclass instance ToSemTokens PosToken (Program Name)

-- Generic instance does not apply because we exclude the level and override
-- the token type for the name.
instance ToSemTokens PosToken (Section Name) where
  toSemTokens (MkSection ann _lvl name decls) =
    flattenSemanticTokens ann [withTokenType nameIsDirective $ toSemTokens name, toSemTokens decls]

deriving anyclass instance ToSemTokens PosToken (TopDecl Name)
deriving anyclass instance ToSemTokens PosToken (LocalDecl Name)
deriving anyclass instance ToSemTokens PosToken (Assume Name)
instance ToSemTokens PosToken (Declare Name) where
  toSemTokens (MkDeclare ann typesig appform decl) =
    flattenSemanticTokens ann [withTokenType identIsType $ toSemTokens typesig, toSemTokens appform, toSemTokens decl]
deriving anyclass instance ToSemTokens PosToken (TypeDecl Name)
deriving anyclass instance ToSemTokens PosToken (ConDecl Name)
instance ToSemTokens PosToken (Type' Name) where
  toSemTokens ty = withTokenType identIsType $ genericToSemTokens ty
deriving anyclass instance ToSemTokens PosToken (TypedName Name)
deriving anyclass instance ToSemTokens PosToken (OptionallyTypedName Name)
deriving anyclass instance ToSemTokens PosToken (OptionallyNamedType Name)
deriving anyclass instance ToSemTokens PosToken (Decide Name)
deriving anyclass instance ToSemTokens PosToken (AppForm Name)
deriving anyclass instance ToSemTokens PosToken (Expr Name)
deriving anyclass instance ToSemTokens PosToken (NamedExpr Name)
deriving anyclass instance ToSemTokens PosToken (Branch Name)
deriving anyclass instance ToSemTokens PosToken (Pattern Name)
deriving anyclass instance ToSemTokens PosToken (TypeSig Name)
deriving anyclass instance ToSemTokens PosToken (GivethSig Name)
deriving anyclass instance ToSemTokens PosToken (GivenSig Name)
deriving anyclass instance ToSemTokens PosToken (Directive Name)

instance ToSemTokens PosToken Int where
  toSemTokens _ = pure []

instance ToSemTokens PosToken Name where
  toSemTokens (MkName ann _) =
    flattenSemanticTokens ann []

instance ToSemTokens PosToken RawName where
  toSemTokens _ = pure []

instance ToSemTokens PosToken Lit where
  toSemTokens (NumericLit ann _) =
    flattenSemanticTokens ann []
  toSemTokens (StringLit ann _) =
    flattenSemanticTokens ann []

instance ToSemTokens PosToken PosToken where
  toSemTokens t = do
    ctx <- ask
    pure $ maybe [] toList (fromSemanticTokenContext ctx t)
