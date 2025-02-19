module Main (main) where

import JLServer
import LSP.Logger
import LSP.Server

main :: IO ()
main = do
  -- Setup the logger
  recorder <- makeDefaultStderrRecorder Nothing
  let
    prettyRecorder = cmapWithPrio pretty recorder
    serverRecorder = cmapWithPrio LogServer prettyRecorder

  -- Get Arguments.
  -- If we wanted to, here is where we would add argument parsing
  args <- getDefaultArguments
  -- Run the Language Server in all its glory!
  serverMain serverRecorder (jl4ServerConfig prettyRecorder) args
