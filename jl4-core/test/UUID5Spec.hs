module UUID5Spec (spec) where

import Test.Hspec
import L4.Crypto.UUID5
import L4.Crypto.SHA1
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Numeric (showHex)

-- Helper to show ByteString as hex
toHex :: BS.ByteString -> String
toHex = concatMap (\b -> let s = showHex b "" in if length s == 1 then '0':s else s) . BS.unpack

spec :: Spec
spec = do
  describe "SHA1" $ do
    -- Test vectors from RFC 3174
    it "hashes empty string correctly" $ do
      -- SHA1("") = da39a3ee5e6b4b0d3255bfef95601890afd80709
      toHex (sha1 "") `shouldBe` "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    
    it "hashes 'abc' correctly" $ do
      -- SHA1("abc") = a9993e364706816aba3e25717850c26c9cd0d89d
      toHex (sha1 "abc") `shouldBe` "a9993e364706816aba3e25717850c26c9cd0d89d"

  describe "UUID5" $ do
    it "generates a valid UUIDv5 format" $ do
      let result = T.unpack $ toText $ generateNamed namespaceURL $ TE.encodeUtf8 "test"
      -- UUID format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
      length result `shouldBe` 36
      -- Check dashes are in correct positions
      result !! 8 `shouldBe` '-'
      result !! 13 `shouldBe` '-'
      result !! 18 `shouldBe` '-'
      result !! 23 `shouldBe` '-'
      -- Version should be 5 (position 14, 0-indexed)
      result !! 14 `shouldBe` '5'
      -- Variant should be 8, 9, a, or b (position 19, 0-indexed)
      result !! 19 `shouldSatisfy` (`elem` ("89ab" :: String))

    it "generates deterministic UUIDs" $ do
      let result1 = toText $ generateNamed namespaceURL $ TE.encodeUtf8 "http://example.com"
      let result2 = toText $ generateNamed namespaceURL $ TE.encodeUtf8 "http://example.com"
      result1 `shouldBe` result2

    it "generates different UUIDs for different inputs" $ do
      let result1 = toText $ generateNamed namespaceURL $ TE.encodeUtf8 "test1"
      let result2 = toText $ generateNamed namespaceURL $ TE.encodeUtf8 "test2"
      result1 `shouldNotBe` result2

    it "generates different UUIDs for different namespaces" $ do
      let result1 = toText $ generateNamed namespaceURL $ TE.encodeUtf8 "test"
      let result2 = toText $ generateNamed namespaceDNS $ TE.encodeUtf8 "test"
      result1 `shouldNotBe` result2
