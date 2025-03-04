{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import L4.CRUD (mkApp, withEnv)
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = withEnv \p env ->
  Warp.run p $ mkApp env
