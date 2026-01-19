{-# LANGUAGE BangPatterns #-}
-- | Pure Haskell UUIDv5 implementation.
--
-- This module provides UUIDv5 (name-based, SHA-1) generation without
-- depending on the @uuid@ package, which requires the @entropy@ library
-- that is not available in WebAssembly builds.
--
-- UUIDv5 is defined in RFC 4122. It generates deterministic UUIDs by
-- hashing a namespace UUID concatenated with a name using SHA-1.
--
-- @since 0.1
module L4.Crypto.UUID5
  ( -- * UUID generation
    generateNamed

    -- * Standard namespaces (from RFC 4122)
  , namespaceURL
  , namespaceDNS
  , namespaceOID
  , namespaceX500

    -- * Conversion
  , toText
  , toByteString
  ) where

import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text (Text)
import Numeric (showHex)

import L4.Crypto.SHA1 (sha1)

-- | A UUID represented as 16 bytes.
newtype UUID5 = UUID5 ByteString
  deriving (Eq, Ord, Show)

-- | Generate a UUIDv5 from a namespace UUID and a name.
--
-- The namespace should be a 16-byte ByteString representing a UUID.
-- Standard namespaces are provided: 'namespaceURL', 'namespaceDNS', etc.
--
-- >>> toText $ generateNamed namespaceURL "http://example.com"
-- "a0c2e86a-c7e3-5579-944f-8b6815cc2736"
generateNamed :: ByteString  -- ^ Namespace UUID (16 bytes)
              -> ByteString  -- ^ Name to hash
              -> UUID5
generateNamed namespace name =
  let -- Hash namespace ++ name with SHA-1
      !hash = sha1 (namespace <> name)

      -- Take first 16 bytes
      !bytes = BS.take 16 hash

      -- Set version to 5 (byte 6, high nibble)
      !byte6 = (BS.index bytes 6 .&. 0x0f) .|. 0x50

      -- Set variant to RFC 4122 (byte 8, bits 6-7 = 10)
      !byte8 = (BS.index bytes 8 .&. 0x3f) .|. 0x80

      -- Reconstruct the UUID with version and variant bits set
      !result = BS.concat
        [ BS.take 6 bytes
        , BS.singleton byte6
        , BS.singleton (BS.index bytes 7)
        , BS.singleton byte8
        , BS.drop 9 bytes
        ]

  in UUID5 result

-- | Convert UUID to standard text representation.
--
-- Format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
toText :: UUID5 -> Text
toText (UUID5 bs) = Text.pack formatted
  where
    hex = concatMap (\b -> pad2 $ showHex b "") (BS.unpack bs)
    formatted =
      take 8 hex ++ "-" ++
      take 4 (drop 8 hex) ++ "-" ++
      take 4 (drop 12 hex) ++ "-" ++
      take 4 (drop 16 hex) ++ "-" ++
      drop 20 hex

    pad2 :: String -> String
    pad2 [c] = ['0', c]
    pad2 s = s

-- | Get the raw 16-byte representation of a UUID.
toByteString :: UUID5 -> ByteString
toByteString (UUID5 bs) = bs

------------------------------------------------------
-- Standard namespaces (RFC 4122, Appendix C)
------------------------------------------------------

-- | Namespace for URLs: 6ba7b811-9dad-11d1-80b4-00c04fd430c8
namespaceURL :: ByteString
namespaceURL = BS.pack
  [ 0x6b, 0xa7, 0xb8, 0x11
  , 0x9d, 0xad
  , 0x11, 0xd1
  , 0x80, 0xb4
  , 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
  ]

-- | Namespace for DNS names: 6ba7b810-9dad-11d1-80b4-00c04fd430c8
namespaceDNS :: ByteString
namespaceDNS = BS.pack
  [ 0x6b, 0xa7, 0xb8, 0x10
  , 0x9d, 0xad
  , 0x11, 0xd1
  , 0x80, 0xb4
  , 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
  ]

-- | Namespace for ISO OIDs: 6ba7b812-9dad-11d1-80b4-00c04fd430c8
namespaceOID :: ByteString
namespaceOID = BS.pack
  [ 0x6b, 0xa7, 0xb8, 0x12
  , 0x9d, 0xad
  , 0x11, 0xd1
  , 0x80, 0xb4
  , 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
  ]

-- | Namespace for X.500 DNs: 6ba7b814-9dad-11d1-80b4-00c04fd430c8
namespaceX500 :: ByteString
namespaceX500 = BS.pack
  [ 0x6b, 0xa7, 0xb8, 0x14
  , 0x9d, 0xad
  , 0x11, 0xd1
  , 0x80, 0xb4
  , 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8
  ]
