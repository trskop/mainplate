{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module:      Options.Applicative.Standard
-- Description: Standard\/common options and arguments used by command-line
--              applications.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Standard\/common options and arguments used by command-line applications.
--
-- Parts were inspired by <http://tldp.org/LDP/abs/html/standard-options.html>
-- and <http://www.catb.org/esr/writings/taoup/html/ch10s05.html>.
module Options.Applicative.Standard
    (
    -- * Version Information
      version
    , versionFlag

    -- * Command Input\/Output
    , output
    , outputOption
    , file

    -- * Configuration File
    , config
    , configOption

    -- * Host and Port
    , listenHostAndPort
    , listenHost
    , listenPort
    , connectHostAndPort

    -- * Verbosity
    , quiet
    , quietFlag
    , silent
    , silentFlag
    , verbose
    , verboseFlag
    , verbosity
    , parseVerbosity
    , verbosityOption
    , incrementVerbosityFlag

    -- * Other
    , batch
    , define
    , message
    )
  where

import Prelude (maxBound, minBound)

import Control.Monad ((>=>))
import Data.Bool (Bool)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.Maybe (maybe)
import Data.Monoid ((<>), mconcat)
import Data.String (String)
import Data.Word (Word)
import Text.Show (show)

import Data.HostAndPort.Class (Host, HasHost, setHost, Port, HasPort, setPort)
import Data.HostAndPort.Parse
    ( ParsedHost
    , modifyHostAndPortWith
    , parseConnect
    , parseHost
    , parseListen
    , parsePort
    )
import Data.Verbosity (Verbosity(Silent, Verbose))
import qualified Data.Verbosity as Verbosity (increment', parse)
import Data.Verbosity.Class (HasVerbosity, modifyVerbosity, setVerbosity)
import Options.Applicative
    ( Mod
    , ParseError(InfoMsg)
    , Parser
    , ReadM
    , abortOption
    , eitherReader
    , flag
    , help
    , hidden
    , long
    , metavar
    , option
    , short
--  , str
    )
import Options.Applicative.Builder.Internal (HasMetavar, HasName)

import Data.ConfigFile (IsConfigFilePath, parseConfigFilePath)
import Data.Version.Class (IsVersion)
import qualified Data.Version.Class as Version (toSomeString)
import Data.Output (IsOutput, parseOutput)


-- {{{ Host and Port ----------------------------------------------------------

-- | Parse argument of the form @HOST_NAME|IPV4_ADDR|IPV6_ADDR@.
--
-- See 'parseHost' for more information.
listenHost
    :: (Host a ~ host, HasHost a)
    => (ParsedHost String -> Either String host)
    -> ReadM (a -> a)
listenHost convertHost =
    eitherReader $ parseHost >=> fmap setHost . convertHost

-- | Parse port number argument. Note that @0@ (zero) is allowed, normally it
-- should be interpreted as "let OS pick the port."
--
-- See 'parsePort' for more information.
listenPort
    :: (Port a ~ port, HasPort a)
    => (Word -> Either String port)
    -> ReadM (a -> a)
listenPort convertPort =
    eitherReader $ parsePort >=> fmap setPort . convertPort

-- | Parse argument in the form of either @HOST[:PORT]@ or @[HOST]:PORT@. In
-- other words, either @HOST@ or @PORT@ has to be provided. Where @PORT@ is a
-- TCP/UDP port number, and @HOST@ is one of:
--
-- * @*@ - Listen on all interfaces, both IPv4 and IPv6.
-- * @0.0.0.0@ - Listen on all interfaces, but IPv4 only.
-- * @[::]@ - Listen on all interfaces, both IPv6 and IPv4.
-- * Valid IPv4 address, e.g. @127.0.0.1@ for IPv4 loopback.
-- * Valid IPv6 address in square brackets, e.g. @[::1]@ for IPv6 loopback.
-- * Valid hostname as defined by
--   <https://tools.ietf.org/html/rfc1123 RFC 1123>, e.g. @\"example.com\"@.
--
-- See 'parseListen' for more information.
listenHostAndPort
    :: (Host a ~ host, Port a ~ port, HasHost a, HasPort a)
    => (ParsedHost String -> Either String host)
    -> (Word -> Either String port)
    -> ReadM (a -> a)
listenHostAndPort convertHost convertPort = eitherReader
    $ parseListen >=> modifyHostAndPortWith convertHost convertPort

{- TODO: Create connect alternatives to listenHost and listenPort.

-- | Parse argument of the form @HOST_NAME|IPV4_ADDR|IPV6_ADDR@.
--
-- See 'parseHost' for more information.
connectHost
    :: (Host a ~ host, HasHost a)
    => (ParsedHost String -> Either String host)
    -> ReadM (a -> a)
connectHost convertHost =
    eitherReader $ parseHost >=> fmap setHost . convertHost

-- | Parse port number argument. Note that @0@ (zero) is allowed, normally it
-- should be interpreted as "let OS pick the port."
--
-- See 'parsePort' for more information.
connectPort
    :: (Port a ~ port, HasPort a)
    => (Word -> Either String port)
    -> ReadM (a -> a)
connectPort convertPort =
    eitherReader $ parsePort >=> fmap setPort . convertPort
-}

-- | Same as 'listenHostAndPort', but:
--
-- * will reject @HOST@ when it's @\*@, @::@, or @0.0.0.0@;
-- * and it will reject @PORT@ when it's set to @0@.
--
-- See 'parseConnect' for more information.
connectHostAndPort
    :: (Host a ~ host, Port a ~ port, HasHost a, HasPort a)
    => (ParsedHost String -> Either String host)
    -> (Word -> Either String port)
    -> ReadM (a -> a)
connectHostAndPort convertHost convertPort = eitherReader
    $ parseConnect >=> modifyHostAndPortWith convertHost convertPort

-- }}} Host and Port ----------------------------------------------------------

-- {{{ Version ----------------------------------------------------------------

-- | Option for printing version information. It's defined either as
-- @-V|--version@ or @-v|--version@ depending on the value of the first
-- argument.
--
-- > -V (or -v), --version
-- >     Print version information and exit.
--
-- Usage example:
--
-- @
-- versionOption = abortOption (InfoMsg \"hyper-command 1.1.1.1\") version
-- @
version
    :: HasName f
    => Bool
    -- ^ Use uppercase 'V'? In other words if 'True' then the short option is
    -- @-V@, and if 'False' then it's @-v@. This is so that version option can
    -- be combined with e.g. verbosity option for which @-v@ is also commonly
    -- used.
    -> Mod f a
version useUpperCase = mconcat
    [ long "version"
    , short $ if useUpperCase then 'V' else 'v'
    , help "Print version information and exit."
    , hidden
    ]

-- | Defined as:
--
-- @
-- 'versionFlag' useUpperCase v =
--     'abortOption' ('InfoMsg' '$' 'Version.toSomeString' v) '$' 'version' useUpperCase
-- @
--
-- See 'version' for more information.
versionFlag
    :: IsVersion v
    => Bool
    -- ^ Use uppercase 'V'? In other words if 'True' then the short option is
    -- @-V@, and if 'False' then it's @-v@. This is so that version option can
    -- be combined with e.g. verbosity option for which @-v@ is also commonly
    -- used.
    -> v
    -- ^ Version information to be printed.
    -> Parser (a -> a)
versionFlag useUpperCase v =
    abortOption (InfoMsg $ Version.toSomeString v) $ version useUpperCase

-- }}} Version ----------------------------------------------------------------

-- {{{ Command Output ---------------------------------------------------------

-- | Option for writing output into a file.
--
-- > -o FILE, --output FILE
-- >     Write output into FILE.
output :: (HasName f, HasMetavar f) => Mod f a
output = mconcat
    [ short 'o'
    , long "output"
    , metavar "FILE"
    , help "Write output into FILE."
    ]

-- | Defined as:
--
-- @
-- 'outputOption' = 'option' ('eitherReader' 'parseOutput') 'output'
-- @
--
-- See 'output' for more details.
outputOption :: IsOutput a => Parser a
outputOption = option (eitherReader parseOutput) output

-- | Option for reading input from a file.
--
-- > -f FILE, --file FILE
-- >     Read input from FILE.
file :: (HasName f, HasMetavar f) => Mod f a
file = mconcat
    [ short 'f'
    , long "file"
    , metavar "FILE"
    , help "Read input from FILE."
    ]

-- }}} Command Output ---------------------------------------------------------

-- {{{ Configuration File -----------------------------------------------------

-- | Option for passing configuration file to an application.
--
-- > -c FILE, --config FILE
-- >     Read FILE as configuration file.
config :: (HasName f, HasMetavar f) => Mod f a
config = mconcat
    [ short 'c'
    , long "config"
    , metavar "FILE"
    , help "Read FILE as configuration file."
    ]

-- | Defined as:
--
-- @
-- 'configOption' = 'option' ('eitherReader' 'parseConfigFilePath') 'config'
-- @
configOption :: IsConfigFilePath a => Parser a
configOption = option (eitherReader parseConfigFilePath) config

-- }}} Configuration File -----------------------------------------------------

-- {{{ Verbosity --------------------------------------------------------------

-- | Option for suppressing unnecessary output.
--
-- > -q, --quiet
-- >     Quiet mode. Suppress normal diagnostic or result output.
quiet :: HasName f => Mod f a
quiet = mconcat
    [ long "quiet"
    , short 'q'
    , help "Quiet mode. Suppress normal diagnostic or result output."
    ]

-- | Defined as:
--
-- @
-- 'quietFlag' = 'flag' 'id' ('setVerbosity' 'Silent') 'quiet'
-- @
--
-- See also 'quiet', 'silent', 'silentFlag', and 'Verbosity'.
quietFlag :: HasVerbosity a => Parser (a -> a)
quietFlag = flag id (setVerbosity Silent) quiet

-- | Option for suppressing unnecessary output.
--
-- > -s, --silent
-- >     Silent mode. Suppress normal diagnostic or result output.
--
-- See also 'silentFlag', 'quiet', 'quietFlag', and 'Verbosity'.
silent :: HasName f => Mod f a
silent = mconcat
    [ long "silent"
    , short 's'
    , help "Silent mode. Suppress normal diagnostic or result output."
    ]

-- | Defined as:
--
-- @
-- 'silentFlag' = 'flag' 'id' ('setVerbosity' 'Silent') 'silent'
-- @
--
-- See also 'silent', 'quiet', 'quietFlag', and 'Verbosity'.
silentFlag :: HasVerbosity a => Parser (a -> a)
silentFlag = flag id (setVerbosity Silent) silent

-- | Option for printing additional diagnostic output.
--
-- > -v, --verbose
-- >     Verbose mode. Prints additional diagnostic output.
verbose :: HasName f => Mod f a
verbose = mconcat
    [ short 'v'
    , long "verbose"
    , help "Verbose mode. Prints additional diagnostic output."
    ]

-- | Defined as:
--
-- @
-- 'verboseFlag' = 'flag' 'id' ('setVerbosity' 'Verbose') 'verbose'
-- @
--
-- See 'verbose' and 'Verbosity' for more details.
verboseFlag :: HasVerbosity a => Parser (a -> a)
verboseFlag = flag id (setVerbosity Verbose) verbose

-- | Option for setting verbosity to a specified value.
--
-- > --verbosity=VERBOSITY
-- >     Set verbosity level to VERBOSITY.
verbosity :: (HasName f, HasMetavar f) => Mod f a
verbosity = mconcat
    [ long "verbosity"
    , metavar "VERBOSITY"
    , help "Set verbosity level to VERBOSITY."
    ]

-- | Parse verbosity value and set it. These are recognized values: "silent",
-- "normal", "verbose", and "annoying".
parseVerbosity :: HasVerbosity a => ReadM (a -> a)
parseVerbosity = eitherReader $ \s ->
    maybe (invalidVerbosity s) (Right . setVerbosity) $ Verbosity.parse s
  where
    invalidVerbosity s = Left
        $ "Invalid verbosity: " <> show s <> ": Verbosity can be only one of: "
        <> show [minBound .. maxBound :: Verbosity]

-- | Defined as:
--
-- @
-- 'verbosityOption' = 'option' 'parseVerbosity' 'verbosity'
-- @
--
-- See 'parseVerbosity' and 'verbosity' for more information.
verbosityOption :: HasVerbosity a => Parser (a -> a)
verbosityOption = option parseVerbosity verbosity

-- | Flag for incrementing verbosity by one level. It can be used multiple
-- times to increase it more.
--
-- > -v
-- >     Increment verbosity by one level. Can be used multiple times.
--
-- See 'Verbosity.increment'' for more details.
incrementVerbosityFlag :: HasVerbosity a => Parser (a -> a)
incrementVerbosityFlag =
    flag id (modifyVerbosity Verbosity.increment') $ mconcat
        [ short 'v'
        , help "Increment verbosity by one level. Can be used multiple times."
        ]

-- }}} Verbosity --------------------------------------------------------------

-- {{{ Other ------------------------------------------------------------------

-- | Flag for switching to batch mode, a non interactive mode which can be used
-- when calling application from a script.
--
-- > -b, --batch
-- >     Batch mode, suppress interaction and unnecessary input/output.
batch :: HasName f => Mod f a
batch = mconcat
    [ short 'b'
    , long "batch"
    , help "Batch mode, suppress interaction and unnecessary input/output."
    ]

-- | Option to pass user-defined variable definitions to an application. For
-- example used by:
--
-- * <https://gcc.gnu.org/onlinedocs/gcc-2.95.2/gcc_2.html#SEC11 GCC pre-processor options>
-- * <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#c-pre-processor-options GHC C pre-processor options>
--
-- > -D NAME[=VALUE], --define NAME[=VALUE]
-- >     Define a variable NAME, optionally with a specified VALUE.
define :: (HasName f, HasMetavar f) => Mod f a
define = mconcat
    [ short 'D'
    , long "define"
    , metavar "NAME[=VALUE]"
    , help "Define a variable NAME, optionally with a specified VALUE."
    ]

-- | Option for passing text of some kind of a message to the application. For
-- example <https://git-scm.com/docs/git-commit#git-commit--mltmsggt git-commit>
-- uses this option to pass commit message.
--
-- > -m TEXT, --message TEXT
-- >     Use TEXT as a message.
message :: (HasName f, HasMetavar f) => Mod f a
message = mconcat
    [ short 'm'
    , long "message"
    , metavar "TEXT"
    , help "Use TEXT as a message."
    ]

-- }}} Other ------------------------------------------------------------------
