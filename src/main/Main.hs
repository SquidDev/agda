{-# LANGUAGE OverloadedStrings #-}
-- | Wrapper for "Agda.Main".
--
-- Agda is installed as a library. This module is used to build the
-- executable.

module Main (main) where

import Control.Monad.State
import Control.Monad.IO.Class
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Text as T
import Data.IORef

import System.IO.Unsafe
import System.IO

import Foreign
import Foreign.C.Types

import Agda.Interaction.Base
import Agda.Interaction.InteractionTop
import Agda.Interaction.JSON hiding (defaultOptions)
import Agda.Interaction.JSONTop
import Agda.Interaction.Options
import Agda.Interaction.Response
import Agda.TypeChecking.Monad
import Agda.Utils.FileName

main :: IO ()
main = pure ()

{-# NOINLINE tcState #-}
-- | The state of the current TCM.
tcState :: IORef TCState
tcState = unsafePerformIO $ newIORef initState

{-# NOINLINE commandState #-}
-- | The state of the current Command state
commandState :: IORef CommandState
commandState = unsafePerformIO $ do
  commands <- newTChanIO
  abort    <- newTVarIO Nothing
  newIORef $ initCommandState (CommandQueue commands abort)

-- | Run the TCM monad in the global state.
runGlobalTCM :: TCM a -> IO a
runGlobalTCM m = unTCM m tcState initEnv

-- | Entry point to set up the initial state.
agdaSetup :: IO ()
agdaSetup = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetEncoding  stdout utf8
  hSetEncoding  stdin  utf8

  runGlobalTCM $ do
    setCommandLineOptions' (AbsolutePath "/") defaultOptions
    setInteractionOutputCallback write

  where
    write :: Response -> TCM ()
    write x = do
      json <- encodeTCM x
      liftIO $ BU.unsafeUseAsCStringLen (BS.toStrict $ encode json) (uncurry agdaInteract)

foreign export ccall agdaSetup :: IO ()

-- | Send a single command to the interactor.
agdaRun :: Ptr CChar -> Int -> IO ()
agdaRun ptr len = do
  command <- parseIOTCM . BC.unpack <$> BU.unsafePackMallocCStringLen (ptr, len)
  case command of
    Left err -> error err
    Right cmd -> do
      state <- readIORef commandState
      state' <- runGlobalTCM $ runCommand cmd `execStateT` state
      writeIORef commandState state'

foreign export ccall agdaRun :: Ptr CChar -> Int -> IO ()

runCommand :: IOTCM -> CommandM ()
runCommand c = do
  commandState <- get
  tcState <- getTC
  tcEnv   <- askTC
  (((), commandState'), tcState')  <- liftIO $ runTCM tcEnv tcState $ runStateT (runInteraction c) commandState
  putTC tcState'
  put commandState'

foreign import ccall agdaInteract :: Ptr CChar -> Int -> IO ()
