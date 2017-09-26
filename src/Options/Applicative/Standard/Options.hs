{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Options.Applicative.Standard.Options
-- Description: Standard/common options used by command line applications.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Standard\/common options used by command line applications.
module Options.Applicative.Standard.Options
    (
    -- * Host and Port
      listenHostAndPort
    , listenHost
    , listenPort
    , connectHostAndPort

    -- * Standard Command-Line Options
    --
    -- Inspired by <http://tldp.org/LDP/abs/html/standard-options.html> and
    -- <http://www.catb.org/esr/writings/taoup/html/ch10s05.html>.
    , version
    , quiet
    , silent
    , output
    , verbose
    )
  where

import Control.Monad ((>=>))
import Data.Bool (Bool)
import Data.Either (Either)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Monoid (mconcat)
import Data.String (String)
import Data.Word (Word)

import Data.HostAndPort.Parse
    ( ParsedHost
    , modifyHostAndPortWith
    , parseConnect
    , parseHost
    , parseListen
    , parsePort
    )
import Data.HostAndPort.Class (Host, HasHost, setHost, Port, HasPort, setPort)
import Options.Applicative
    ( Mod
--  , ParseError(InfoMsg)
--  , Parser
    , ReadM
--  , abortOption
    , eitherReader
    , help
    , hidden
    , long
    , metavar
    , short
    )
import Options.Applicative.Builder.Internal (HasMetavar, HasName)


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

-- | Option for printing version information. It's defined either as
-- @-V|--version@ or @-v|--version@ depending on the value of the first
-- argument.
--
-- @
-- -V (or -v), --version
--   Print version information and exit.
-- @
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

-- | Option for suppressing unnecessary output.
--
-- @
-- -q, --quiet
--   Quiet mode. Suppress normal diagnostic or result output.
-- @
quiet :: HasName f => Mod f a
quiet = mconcat
    [ long "quiet"
    , short 'q'
    , help "Quiet mode. Suppress normal diagnostic or result output."
    ]

-- | Option for suppressing unnecessary output.
--
-- @
-- -s, --silent
--   Silent mode. Suppress normal diagnostic or result output.
-- @
silent :: HasName f => Mod f a
silent = mconcat
    [ long "silent"
    , short 's'
    , help "Silent mode. Suppress normal diagnostic or result output."
    ]

-- | Option for writing output into a file.
--
-- @
-- -o FILE
--   Write output into FILE.
-- @
output :: (HasName f, HasMetavar f) => Mod f a
output = mconcat
    [ short 'o'
    , metavar "FILE"
    , help "Write output into FILE."
    ]

-- | Option for printing additional diagnostic output.
--
-- @
-- -v, --verbose
--   Verbose mode. Prints additional diagnostic output.
-- @
verbose :: HasName f => Mod f a
verbose = mconcat
    [ short 'v'
    , long "verbose"
    , help "Verbose mode. Prints additional diagnostic output."
    ]
