{-# LANGUAGE BangPatterns #-}
-- | Pure Haskell SHA-1 implementation.
--
-- This module provides a SHA-1 hash function implemented entirely in pure
-- Haskell, with no external C dependencies. This makes it suitable for
-- use in WebAssembly builds where system libraries may not be available.
--
-- Note: SHA-1 is considered cryptographically weak and should not be used
-- for security-sensitive applications. This implementation is used only
-- for generating stable UUIDv5 identifiers for caching purposes.
--
-- @since 0.1
module L4.Crypto.SHA1
  ( sha1
  , sha1Lazy
  ) where

import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)

-- | Compute SHA-1 hash of a strict ByteString.
-- Returns a 20-byte (160-bit) hash.
sha1 :: ByteString -> ByteString
sha1 = sha1Lazy . LBS.fromStrict

-- | Compute SHA-1 hash of a lazy ByteString.
-- Returns a 20-byte (160-bit) hash.
sha1Lazy :: LBS.ByteString -> ByteString
sha1Lazy msg = encode finalState
  where
    -- Initial hash values (first 32 bits of fractional parts of square roots of first 5 primes)
    initialState :: (Word32, Word32, Word32, Word32, Word32)
    initialState = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0)

    -- Pad the message according to SHA-1 spec
    paddedMsg = padMessage msg

    -- Process each 512-bit (64-byte) block
    -- First convert to strict and split into blocks
    allBlocks = toBlocks (LBS.toStrict paddedMsg)
    finalState = foldl' processBlock initialState allBlocks

-- | Pad message to multiple of 512 bits (64 bytes)
padMessage :: LBS.ByteString -> LBS.ByteString
padMessage msg = msg <> padding
  where
    msgLen = LBS.length msg
    -- We need: msgLen + 1 + padLen + 8 ≡ 0 (mod 64)
    -- So: padLen = (55 - msgLen) mod 64
    padLen = (55 - fromIntegral msgLen) `mod` 64
    -- Padding: 0x80 followed by zeros, then 64-bit big-endian length
    padding = LBS.pack $
      [0x80] ++
      replicate padLen 0x00 ++
      encodeLength (fromIntegral msgLen * 8)

    encodeLength :: Word64 -> [Word8]
    encodeLength n =
      [ fromIntegral (n `shiftR` 56)
      , fromIntegral (n `shiftR` 48)
      , fromIntegral (n `shiftR` 40)
      , fromIntegral (n `shiftR` 32)
      , fromIntegral (n `shiftR` 24)
      , fromIntegral (n `shiftR` 16)
      , fromIntegral (n `shiftR` 8)
      , fromIntegral n
      ]

-- | Split a ByteString into 64-byte blocks
toBlocks :: ByteString -> [ByteString]
toBlocks bs
  | BS.null bs = []
  | otherwise  = let (block, rest) = BS.splitAt 64 bs
                 in block : toBlocks rest

-- | Process a single 512-bit block
processBlock :: (Word32, Word32, Word32, Word32, Word32)
             -> ByteString
             -> (Word32, Word32, Word32, Word32, Word32)
processBlock (!h0, !h1, !h2, !h3, !h4) block =
  let -- Expand 16 words to 80 words
      w = expandWords block

      -- 80 rounds
      (!a, !b, !c, !d, !e) = foldl' (sha1Round w) (h0, h1, h2, h3, h4) [0..79]

  in (h0 + a, h1 + b, h2 + c, h3 + d, h4 + e)

-- | Expand 16 32-bit words to 80 words
expandWords :: ByteString -> Int -> Word32
expandWords block i
  | i < 16    = getWord32BE block (i * 4)
  | otherwise = rotateL (expandWords block (i-3) `xor`
                         expandWords block (i-8) `xor`
                         expandWords block (i-14) `xor`
                         expandWords block (i-16)) 1

-- | Get a big-endian Word32 from ByteString at given offset
getWord32BE :: ByteString -> Int -> Word32
getWord32BE bs offset =
  let b0 = fromIntegral (BS.index bs offset) `shiftL` 24
      b1 = fromIntegral (BS.index bs (offset + 1)) `shiftL` 16
      b2 = fromIntegral (BS.index bs (offset + 2)) `shiftL` 8
      b3 = fromIntegral (BS.index bs (offset + 3))
  in b0 .|. b1 .|. b2 .|. b3

-- | One round of SHA-1
sha1Round :: (Int -> Word32)  -- ^ Word schedule
          -> (Word32, Word32, Word32, Word32, Word32)
          -> Int
          -> (Word32, Word32, Word32, Word32, Word32)
sha1Round w (!a, !b, !c, !d, !e) i =
  let (!f, !k) = roundConstants i b c d
      temp = rotateL a 5 + f + e + k + w i
  in (temp, a, rotateL b 30, c, d)

-- | Round constants and mixing function based on round number
roundConstants :: Int -> Word32 -> Word32 -> Word32 -> (Word32, Word32)
roundConstants i b c d
  | i < 20    = ((b .&. c) .|. (complement b .&. d), 0x5a827999)
  | i < 40    = (b `xor` c `xor` d, 0x6ed9eba1)
  | i < 60    = ((b .&. c) .|. (b .&. d) .|. (c .&. d), 0x8f1bbcdc)
  | otherwise = (b `xor` c `xor` d, 0xca62c1d6)

-- | Encode final state as ByteString
encode :: (Word32, Word32, Word32, Word32, Word32) -> ByteString
encode (h0, h1, h2, h3, h4) = BS.pack $
  encodeWord32BE h0 ++
  encodeWord32BE h1 ++
  encodeWord32BE h2 ++
  encodeWord32BE h3 ++
  encodeWord32BE h4

encodeWord32BE :: Word32 -> [Word8]
encodeWord32BE w =
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]
