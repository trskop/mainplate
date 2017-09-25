{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:      Main.Config
-- Description: Common patterns used for application @main@.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Common patterns used for application @main@.
module Main.Run
    (
    -- * Command Line and Configuration Files
      ReadConfigException(..)
    , runAppWith

    -- * Application Runtime
    , InitAppRuntimeException(..)
    , withRuntime
    )
  where

import Control.Applicative (Applicative, pure)
import Control.Exception (Exception, SomeException, catch, finally, throw)
import Control.Monad ((>>=))
import Data.Either (Either(Left, Right), either)
import Data.Function ((.))
import Data.Functor (Functor, (<$>))
import Data.Monoid (Endo(appEndo))
import Data.String (String)
import System.IO (IO)
import Text.Show (Show(showsPrec), showString)


-- | Exception thrown when a configuration file can not be read or parsed.
--
-- There are two possible values:
--
-- * @'ReadConfigException' ('Left' exception)@ where @exception@ was thrown
--   by code for reading and parsing configuration file.
--
-- * @'ReadConfigException' ('Right' message)@ where @message@ was returned
--   by the code for reading and parsing configuration file as its error
--   result.
--
-- Examples of how error messages may look like:
--
-- >>> :{
-- >>> error "14:9: Parsing failed on 'foo'."
-- >>>    `catch` (pure . ReadConfigException . Left)
-- >>> :}
-- Failed to read configuration file: 14:9: Parsing failed on 'foo'.
-- CallStack (from HasCallStack):
--   error, called at <interactive>:11:1 in interactive:Ghci2
--
-- >>> :{
-- >>> throw (ReadConfigException $ Right "14:9: Parsing failed on 'foo'.")
-- >>>     `catch` (print :: SomeException -> IO ())
-- >>> :}
-- Failed to read configuration file: 14:9: Parsing failed on 'foo'.
newtype ReadConfigException = ReadConfigException (Either SomeException String)

instance Show ReadConfigException where
    showsPrec d (ReadConfigException e) =
        showString "Failed to read configuration file: "
        .  either (showsPrec d) showString e

instance Exception ReadConfigException

-- | Represents commonly used pattern for command line applications that use
-- configuration file, which can also be specified on command line.
--
-- >      ┌───────────────┐
-- >      │ Parse options │
-- >      └───────┬───────┘
-- >              │
-- >              │ Endo (mode config)
-- >              │
-- >              V
-- >      ┌───────────────┐
-- >      │ Apply default │
-- >      │ configuration │
-- >      └───────┬───────┘
-- >              │
-- >              │ mode config
-- >              │
-- >              V
-- >    ┌───────────────────┐
-- >    │ Application mode, │
-- >    │      options,     │──────┐
-- >    │ and configuration │      │
-- >    └─────────┬─────────┘      │ forall a. mode a
-- >              │                │
-- >              │                V
-- >              │     ┌─────────────────────┐
-- >  mode config │     │     Read & parse    │
-- >              │     │ configuration  file │
-- >              │     └─────────────────────┘
-- >              │                │
-- >              V                │  Either String (Endo config)
-- >   ┌─────────────────────┐     │
-- >   │ Merge configuration │<────┘
-- >   └──────────┬──────────┘
-- >              │
-- >              │ mode config
-- >              │
-- >              V
-- >   ┌─────────────────────┐
-- >   │   Run application   │
-- >   └─────────────────────┘
--
-- 'Functor' @mode@ represents application execution mode. For example types
-- used by an application may look like:
--
-- @
-- data AppMode a
--     = ShowManPage
--     -- ^ Show manual page for this application, and exit afterwards.
--     | RunApp (NonConfigOptions a)
--     -- ^ Run this application with provided configuration.
--   deriving ('Functor', 'Show')
--
-- data NonConfigOptions a = NonConfigOptions
--     { configFile :: 'FilePath'
--     , config :: a
--     }
--   deriving ('Functor', 'Show')
--
-- data Config = Config
--     { ...
--     }
--   deriving ('Show')
--
-- -- So that we can use JSON or YAML files as configuration files.
-- instance FromJSON Config where ...
--
-- defaultConfig :: Config
-- defaultConfig = ...
--
-- defaultAppMode :: AppMode Config
-- defaultAppMode = RunApp NonConfigOptions
--     { configFile = \".\/app.conf\"
--     , config = defaultConfig
--     }
-- @
--
-- Usage examples:
--
-- @
-- main :: 'IO' ()
-- main = 'runAppWith' parseOptions readConfig defaultAppMode '$' \\case
--     ShowManPage -> 'System.Environment.getProgName' >>= runMan
--     RunApp NonConfigOptions{config} -> appMain config
-- @
--
-- @
-- main :: 'IO' ()
-- main = do
--     -- Global configuration may be broader then user configuration, e.g. it
--     -- may contain things that can be used in user configuration.
--     defaultAppMode <- readGlobalConfig
--     'runAppWith' parseOptions readConfig defaultAppMode '$'  \\case
--         ShowManPage -> 'System.Environment.getProgName' >>= runMan
--         RunApp NonConfigOptions{config} -> appMain config
-- @
runAppWith
    :: forall mode config
    .  Functor mode
    => IO (Endo (mode config))
    -- ^ Parse command line arguments into a function that modifies default
    -- application mode and produces what user specified on a command line.
    -> (forall a. mode a -> IO (Either String (Endo config)))
    -- ^ Read configuration file, parse it, and produce a function that
    -- modifies existing configuration. The use of @forall a. mode a@ means
    -- that the reading function is prohibited to depend on current
    -- configuration, i.e. the information in @mode@ functor must be sufficient
    -- for it to find and parse configuration file.
    --
    -- When function returns @Left String@ then the string is wrapped in
    -- 'ReadConfigException' and thrown. If the function throws an excepthin,
    -- then that exception is also wrapped in 'ReadConfigException'. Purpose of
    -- this is to add more meaning to the error\/exception.
    -> mode config
    -- ^ Default application mode and configuration.
    -> (mode config -> IO ())
    -- ^ Application main that takes mode and configuration.
    -> IO ()
runAppWith parseOptions readConfig def appMain =
    parseOptions >>= (`appEndoA` def) >>= readAndApplyConfig >>= appMain
  where
    readAndApplyConfig mode = (\e -> appEndo e <$> mode) <$> readConfig' mode

    readConfig' mode =
        (readConfig mode >>= fromLeftA (readConfigException . Right))
            `catch` (readConfigException . Left)

    appEndoA = (pure .) . appEndo
    readConfigException = throw . ReadConfigException

-- | Exception thrown when an application runtime failed to initialise.
--
-- There are two possible values:
--
-- * @'InitAppRuntimeException' ('Left' exception)@ where @exception@ was
--   thrown by the runtime initialisation code.
--
-- * @'InitAppRuntimeException' ('Right' message)@ where @message@ was returned
--   by the runtime initialisation code as its error result.
--
-- Examples of how error messages may look like:
--
-- >>> :{
-- >>> error "Unable to connect to example.com:5432: ."
-- >>>    `catch` (pure . ReadConfigException . Left)
-- >>> :}
-- Failed to read configuration file: 14:9: Parsing failed on 'foo'.
-- CallStack (from HasCallStack):
--   error, called at <interactive>:11:1 in interactive:Ghci2
--
-- >>> :{
-- >>> throw (ReadConfigException $ Right "14:9: Parsing failed on 'foo'.")
-- >>>     `catch` (print :: SomeException -> IO ())
-- >>> :}
-- Failed to read configuration file: 14:9: Parsing failed on 'foo'.
newtype InitAppRuntimeException =
    InitAppRuntimeException (Either SomeException String)

instance Show InitAppRuntimeException where
    showsPrec d (InitAppRuntimeException e) =
        showString "Failed to initialise application runtime: "
        . either (showsPrec d) showString e

instance Exception InitAppRuntimeException

-- | Pattern for applications that use runtime configuration \/ application
-- runtime, i.e. in-memory parameters that can hold cached values, connection
-- pools, message channels, etc.
--
-- Applications may also want to modify runtime configuration at runtime. For
-- example, GUI application is reconfigured by a user, and runtime
-- configuration must change to reflect it. This way the application doesn't
-- have to be restarted. However, reconfiguration is not handled by this
-- scafolding function. It just ilustrates why something like runtime
-- configuration may be useful.
--
-- Usage example:
--
-- @
-- main :: 'IO' ()
-- main = 'runAppWith' parseOptions readConfig defaultAppMode '$' \\case
--     ShowManPage -> 'System.Environment.getProgName' >>= runMan
--     RunApp NonConfigOptions{config} ->
--         withRuntime initRuntime destroyRuntime config '$' \\runtime -> do
--             ...
-- @
withRuntime
    :: forall config runtime
    .  (config -> IO (Either String runtime))
    -- ^ Initialise runtime of the application from current configuration.
    -> (runtime -> IO ())
    -- ^ Destroy runtime resources before application terminates.
    -> config
    -> (runtime -> IO ())
    -> IO ()
withRuntime init destroy cfg app = do
    runtime <- (init cfg >>= fromLeftA (initAppRuntimeException . Right))
        `catch` (initAppRuntimeException . Left)

    app runtime `finally` destroy runtime
  where
    initAppRuntimeException = throw . InitAppRuntimeException

fromLeftA :: Applicative f => (a -> f b) -> Either a b -> f b
fromLeftA f = either f pure
