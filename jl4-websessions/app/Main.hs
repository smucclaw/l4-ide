module Main (main) where

import L4.CRUD (mkApp, withEnv)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import System.IO

main :: IO ()
main = withEnv \p l env -> do
    hSetBuffering stdout LineBuffering
    let s = Warp.setHost "*" $ Warp.setLogger l $ Warp.setPort p Warp.defaultSettings
    putStrLn $ "Started server on port: " <> show p
    Warp.runSettings s $ corsMiddleware $ mkApp env

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just simpleCorsResourcePolicy
  { corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["content-type"]
  })
