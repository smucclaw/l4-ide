{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module L4.Instances.Serialise () where

#if defined(SERIALISE_ENABLED)
import Codec.Serialise (Serialise (..))
import L4.Annotation (Anno_ (..), emptyAnno)
import Language.LSP.Protocol.Types (NormalizedUri, Uri (..), fromNormalizedUri, toNormalizedUri)

-- | Serialize 'Uri' via its 'Text' payload.
instance Serialise Uri where
  encode (Uri t) = encode t
  decode = Uri <$> decode

-- | Serialize 'NormalizedUri' by round-tripping through 'Uri'.
instance Serialise NormalizedUri where
  encode = encode . fromNormalizedUri
  decode = toNormalizedUri <$> decode

-- | Annotations carry PosTokens (source positions) needed for IDE features but
-- not for evaluation. Strip them during serialization to keep bundles small.
instance (Monoid e) => Serialise (Anno_ t e) where
  encode _ = encode ()
  decode = emptyAnno <$ decode @()
#endif
