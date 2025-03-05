module Main (main) where

import L4.CRUD (mkApp, withEnv)
import qualified Network.Wai.Handler.Warp as Warp
import System.IO

main :: IO ()
main = withEnv \p l env -> do
    hSetBuffering stdout LineBuffering
    let s = Warp.setLogger l $ Warp.setPort p Warp.defaultSettings
    putStrLn $ "Started server on port: " <> show p
    Warp.runSettings s $ mkApp env
