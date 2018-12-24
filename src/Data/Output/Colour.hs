{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Data.Output.Colour
-- Description: Data type representing user preferences for using colourised
--              output.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Data type representing user preferences for using colourised, usually
-- terminal, output.
module Data.Output.Colour
    ( ColourOutput(..)
    , toString
    , parse
    , useColoursWhen

    -- * Environment
    , noColorEnvVar

    -- * Terminal
    , terminalSupportsColours
    )
  where

import Control.Applicative (Applicative, pure)
import Data.Bool (Bool(False, True))
import Data.Eq (Eq)
import Data.Function (const)
import Data.Functor ((<$>), fmap)
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.String (IsString)
import GHC.Generics (Generic)
import Text.Show (Show)

import qualified Dhall (Inject, Interpret)
import System.Console.Terminfo (Terminal, getCapability, termColors)

import System.Environment.Parser (ParseEnv, optionalVar)


-- | Many command-line tools provide option like @--colour=WHEN@ which allows
-- users to specify when the tool should produce colourised output.  Usually
-- output printed to an ANSI terminal enriched with ANSI escape sequences that
-- are interpreted by the terminal as colours.
--
-- This data type specifies the @WHEN@ values in @--colour=WHEN@, and it uses
-- the same terminology as it is used by many existing tools.
data ColourOutput
    = Always
    -- ^ Always use colourised output, even if output is not a terminal. This
    -- can be useful when, for example, piping output to a pager.
    | Auto
    -- ^ Use colourised output when the output is a terminal that supports it.
    | Never
    -- ^ Never produce colourised output.
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Dhall.Inject, Dhall.Interpret)

-- | Show 'ColourOutput' as a (generic) lower-case string.  It is possible to
-- use 'Show' instance of 'ColourOutput' instead, however, this function is
-- more consistent with how 'parse' operates.
--
-- >>> toString Always :: String
-- "always"
-- >>> toString Auto :: String
-- "auto"
-- >>> toString Never :: String
-- "never"
--
-- See also 'parse'.
toString :: IsString s => ColourOutput -> s
toString = \case
    Always -> "always"
    Auto -> "auto"
    Never -> "never"

-- | Parse a string representation of 'ColourOutput'.  This differs from what
-- 'Show' is producing since by default we are parsing lower-case version
-- (@\"always\"@, @\"auto\"@, and @\"never\"@), which is more suitable for
-- command line options.
--
-- >>> import Data.Output.Colour as ColourOutput
-- >>> ColourOutput.parse "never"
-- Just Never
--
-- >>> import Data.Output.Colour as ColourOutput
-- >>> ColourOutput.parse "invalid"
-- Nothing
--
-- >>> import Data.Output.Colour as ColourOutput
-- >>> ColourOutput.parse "Never"
-- Nothing
--
-- Use <https://hackage.haskell.org/package/case-insensitive case-insensitive>
-- package to make this function case insensitive:
--
-- >>> import Data.Output.Colour as ColourOutput
-- >>> import qualified Data.CaseInsensitive as CI (mk)
-- >>> ColourOutput.parse (CI.mk "Auto")
-- Just Auto
--
-- See also 'toString'.
parse :: (IsString s, Eq s) => s -> Maybe ColourOutput
parse = \case
    "always" -> Just Always
    "auto" -> Just Auto
    "never" -> Just Never
    _ -> Nothing

-- | Check for presence of @NO_COLOR@ environment variable. If present, then
-- set `ColourOutput` value to `Never`, otherwise keep it as it was.
--
-- @NO_COLOR@ einvironment variable is an informal standard which is available
-- online at <https://no-color.org>.
--
-- > Accepting the futility of trying to reverse this trend, an informal
-- > standard is hereby proposed:
-- >
-- > > All command-line software which outputs text with ANSI color added
-- > > should check for the presence of a NO_COLOR environment variable that,
-- > > when present (regardless of its value), prevents the addition of ANSI
-- > > color.
--
-- To turn this into 'Bool' just use:
--
-- @
-- -- False - @NO_COLOR@ environment variable not present, feel free to use
-- --         colours.
-- -- True  - @NO_COLOR@ environment variable is defined, don't use colours.
-- 'isJust' \<$> 'noColorEnvVar' :: 'ParseEnv' context 'Bool'
--
-- -- or
--
-- -- False - @NO_COLOR@ environment variable is defined, don't use colours.
-- -- True  - @NO_COLOR@ environment variable not present, feel free to use
-- --         colours.
-- 'Data.Maybe.isNothing' \<$> 'noColorEnvVar' :: 'ParseEnv' context 'Bool'
-- @
--
-- If @NO_COLOR@ should override existing 'ColourOutput' value, usually when
-- reading a configuration file, then use:
--
-- @
-- useColoursInOutput
--     :: 'ColourOutput'                   -- Taken from e.g. configuration.
--     -> 'ParseEnv' context 'ColourOutput'
-- useColoursInOutput colourOutput =
--     'Data.Maybe.fromMaybe' colourOutput \<$> 'noColorEnvVar'
-- @
noColorEnvVar
    :: ParseEnv context (Maybe ColourOutput)
    -- ^
    -- * 'Nothing' if @NO_COLOR@ environment variable is not defined.
    -- * @'Just' 'Never'@ if @NO_COLOR@ environment variable is defined.
noColorEnvVar = fmap (const Never) <$> optionalVar "NO_COLOR"

-- | Does specified terminal support colours?
terminalSupportsColours
    :: Terminal
    -- ^ Entry from @terminfo@, see 'System.Console.Terminfo.Base.setupTerm',
    -- and 'System.Console.Terminfo.Base.setupTermFromEnv'.
    -> Bool
    -- ^
    -- * 'False' - No, the terminal doesn't support colours.
    -- * 'True' - Yes, the terminal does support colours.
terminalSupportsColours term = isJust (getCapability term termColors)

-- | Evaluate 'ColourOutput' by resolving 'Auto' case using provided predicate.
--
-- Examples:
--
-- @
-- 'useColoursWhen' 'terminalSupportsColours'
--    :: 'ColourOutput' -> 'Terminal' -> 'Bool'
--
-- 'useColoursWhen' ('terminalSupportsColours' \<$> 'System.Console.Terminfo.Base.setupTermFromEnv')
--    :: 'ColourOutput' -> IO 'Bool'
-- @
useColoursWhen
    :: Applicative f
    => f Bool
    -- ^ Determine if colours should be used in case of 'Auto':
    --
    -- * 'False' - Don't use colours if 'ColourOutput' value is 'Auto'.
    -- * 'True' - Use colours if 'ColourOutput' value is 'Auto'.
    --
    -- See also 'terminalSupportsColours'.
    -> ColourOutput
    -> f Bool
    -- ^ Use colours in the output?
    --
    -- * 'False' - No, don't use colours in the output.
    -- * 'True' - Yes, use colours in the output.
useColoursWhen figureItOut = \case
    Always -> pure True
    Auto -> figureItOut
    Never -> pure False
