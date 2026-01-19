{-# LANGUAGE OverloadedStrings #-}
-- | Main module for the L4 WASM executable.
--
-- This module imports L4.Wasm which contains the foreign export declarations.
-- The foreign exports are automatically linked when compiling with -static.
module Main (main) where

-- Import L4.Wasm to ensure its code (including foreign exports) is linked.
-- We must reference at least one exported function to force linking.
import L4.Wasm (l4Check)

-- | Entry point for the WASM reactor.
-- We reference l4Check to force the L4.Wasm module (and its foreign exports) to be linked.
main :: IO ()
main = l4Check "" `seq` pure ()
