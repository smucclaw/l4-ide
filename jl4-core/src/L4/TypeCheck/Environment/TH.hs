{-# LANGUAGE TemplateHaskell #-}
module L4.TypeCheck.Environment.TH (preDef, mkBuiltins, builtinUri, rename, variants) where

import Base
import Language.Haskell.TH.Syntax
import qualified Data.Char as Char

import L4.Annotation
import L4.Syntax as L4
import L4.Lexer
import L4.Parser.SrcSpan

data Renamed = MkRenamed
  { orig :: String
  , chosen :: Maybe String
  , variants :: Maybe [String]
  -- ^ There cannot be more than 10 variants for the same name
  -- at the moment.
  -- This is an arbitrary cut-off, that is easily fixed.
  }
  deriving stock (Eq, Show)

rename :: Renamed -> String -> Renamed
rename r s = MkRenamed r.orig (Just s) Nothing

variants :: Renamed -> [String] -> Renamed
variants r vs = r { variants = Just vs }

instance IsString Renamed where
  fromString a = MkRenamed a Nothing Nothing

-- | Helper function to create a predefined name.
preDef :: Text -> L4.Name
preDef t =
  MkName
    (mkAnno
      [mkCluster
        (CsnCluster
          (ConcreteSyntaxNode [MkPosToken (MkSrcRange (MkSrcPos 0 0) (MkSrcPos 0 0) 0 builtinUri) (TIdentifier t)] Nothing Visible)
          (ConcreteSyntaxNode [] Nothing Hidden)
        )
      ]
    )
    (NormalName t)

mkBuiltins :: [Renamed] -> Q [Dec]
mkBuiltins = fmap mconcat . traverse mkBuiltin . zip [1..]

builtinUri :: NormalizedUri
builtinUri = toNormalizedUri (Uri "jl4:builtin")

mkBuiltin :: (Int, Renamed) -> Q [Dec]
mkBuiltin (i, MkRenamed name o vars) = do

  nameName <- newName (name <> "Name")
  nameExp <- [| preDef $(pure $ LitE $ StringL $ fromMaybe (map Char.toUpper name) o) |]

  defs <- forM (zip [(0 :: Int) ..] $ fromMaybe [""] vars) \(j, v) -> do
    unqName <- newName (name <> v <> "Unique")
    unqExp <- [| MkUnique 'b' (10 * i + j) builtinUri |]

    let unqVar = pure $ VarE unqName
        nameVar = pure $ VarE nameName

    defName <- newName (name <> v <> "Def")
    defExp <- [| Def $unqVar $nameVar |]

    refName <- newName (name <> v <> "Ref")
    refExp <- [| Ref $nameVar $unqVar $nameVar |]
    pure
      [ SigD unqName (ConT ''Unique)
      , SigD defName (ConT ''Resolved)
      , SigD refName (ConT ''Resolved)
      , FunD unqName [Clause [] (NormalB unqExp) []]
      , FunD defName [Clause [] (NormalB defExp) []]
      , FunD refName [Clause [] (NormalB refExp) []]
      ]

  pure $
    [ SigD nameName (ConT ''L4.Name)
    , FunD nameName [Clause [] (NormalB nameExp) []]
    ] <> concat defs

