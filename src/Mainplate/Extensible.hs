-- |
-- Module:      Mainplate.Extensible
-- Description: Extensible application where subcommands can be added using
--              separate executables.
-- Copyright:   (c) 2017-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Extensible application where subcommands can be added using separate
-- executables.
module Mainplate.Extensible
    (
      Command(..)
    , runExtensibleAppWith

    -- * External Command
    , ExternalCommand(..)
    , runExternalCommand

    -- ** Process
    , externalCommandToCreateProcess
    , runExternalProcess
    , handleExitCode
    )
  where

import Control.Monad ((>=>))
import Control.Exception (SomeException, throwIO)
import Data.Either (Either)
import Data.Eq (Eq)
import Data.Functor (Functor)
import Data.Function ((.))
import Data.Monoid (Endo)
import Data.String (String)
import GHC.Generics (Generic, Generic1)
import System.Exit (ExitCode)
import System.IO (IO)
import Text.Show (Show)

import System.Process (CreateProcess, proc, waitForProcess, withCreateProcess)

import Mainplate.Core (runAppWith)


-- | Application is either running 'Internal' acction or 'External' action.
-- 'External' actions are usually special executables that are used as plugins.
data Command externalAction internalAction conf
    = Internal internalAction conf
    -- ^ Represents action that is provided internally by main
    -- application\/executable.
    | External externalAction conf
    -- ^ Represents action that is provided by an external application.
  deriving (Eq, Functor, Generic, Generic1, Show)
    -- TODO: Instances for Show1 and Eq1?

-- | Template for application that uses external processes as extensions.
--
-- This is just a specialised version of 'runAppWith', and most of its
-- arguments are preserved. See its documentation for more details.
runExtensibleAppWith
    :: forall externalAction internalAction config
    .  IO (Endo (Command externalAction internalAction config))
    -- ^ Parse command line arguments.
    ->  ( forall a. Command externalAction internalAction a
        -> IO (Either String (Endo config))
        )
    -- ^ Read configuration file.
    ->  ( Endo (Command externalAction internalAction config)
        -> IO (Command externalAction internalAction config)
        )
    -- ^ Apply defaults.
    -> (externalAction -> config -> IO ())
    -- ^ Run action provided by an external application.
    -> (internalAction -> config -> IO ())
    -- ^ Run action that is implemented internally.
    -> IO ()
runExtensibleAppWith parseOpts appDefaults readConfig runExternal runInternal =
    runAppWith parseOpts appDefaults readConfig \case
        Internal internalAction config -> runInternal internalAction config
        External externalAction config -> runExternal externalAction config

-- {{{ ExternalCommand --------------------------------------------------------

-- | Description of external command to be executed.
data ExternalCommand = ExternalCommand
    { executable :: String
    , options :: [String]
    }
  deriving (Eq, Generic, Show)

-- | Combinator for running external command.
runExternalCommand
    :: (cmd -> IO ())
    -- ^ Run external command.
    -> (ExternalCommand -> config -> IO cmd)
    -- ^ Transform 'ExternalCommand' and @config@ in to a command description
    -- that can be executed.
    -> ExternalCommand
    -> config
    -> IO ()
runExternalCommand run f cmd = f cmd >=> run
    -- TODO: Cleanup! Function @f@ may allocate resources that may need cleanup.

-- {{{ ExternalCommand -- Process ---------------------------------------------

-- | See 'proc' for more information.
--
-- Usage example:
--
-- @
-- mkCreateProcess :: 'ExternalCommand' -> 'CreateProcess'
-- mkCreateProcess extCmd = ('externalCommandToCreateProcess' extCmd)
--     { ...
--     }
-- @
--
-- See 'CreateProcess' documentation for more information.
externalCommandToCreateProcess :: ExternalCommand -> CreateProcess
externalCommandToCreateProcess ExternalCommand{..} = proc executable options

-- | Specialised version of 'runExternalCommand' that uses
-- <http://hackage.haskell.org/package/process process> package to execute
-- external command.
--
-- Usage example:
--
-- @
-- runExternal
--     :: 'ExternalCommand'
--     -> Config
--     -> IO ('CreateProcess', 'ExitCode' -> IO ())
-- runExternal = 'runExternalProcess' $ \\extCmd cfg ->
--     (,) '<$>' mkCreateProcess extCmd cfg '<*>' mkHandleExcitCode extCmd cfg
--   where
--     mkCreateProcess extCmd cfg = do
--         ...
--         pure ('externalCommandToCreateProcess' extCmd)
--             { ...
--             }
--
--     mkHandleExcitCode extCmd cfg = do
--         ...
--         pure $ \\case
--             ExitSuccess -> pure ()
--             ec\@(ExitFailure _) -> do
--                 -- Error handling, logging, etc.
--                 ...
--                 -- Following will throw an exception:
--                 'handleExitCode' (toException . ExternalCommandFailed) extCmd
-- @
runExternalProcess
    :: (ExternalCommand -> config -> IO (CreateProcess, ExitCode -> IO ()))
    -> ExternalCommand
    -> config
    -> IO ()
runExternalProcess = runExternalCommand \(createProcess, processExitCode) ->
    withCreateProcess createProcess \_ _ _ ->
        waitForProcess >=> processExitCode

-- | Helper function for processing 'ExitCode' returned by an external command.
handleExitCode :: (ExitCode -> SomeException) -> ExitCode -> IO ()
handleExitCode f = throwIO . f

-- }}} ExternalCommand -- Process ---------------------------------------------

-- }}} ExternalCommand --------------------------------------------------------
