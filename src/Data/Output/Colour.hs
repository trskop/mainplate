{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Data.Output.Colour
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Data.Output.Colour
    ( ColourOutput(..)
    , parse

    -- * Environment
    , noColorEnvVar

    -- * Terminal
    , terminalSupportsColours
    )
  where

import Data.Bool (Bool)
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
noColorEnvVar :: ParseEnv context (Maybe ColourOutput)
noColorEnvVar = fmap (const Never) <$> optionalVar "NO_COLOR"

-- | Does specified terminal support colours?
terminalSupportsColours :: Terminal -> Bool
terminalSupportsColours term = isJust (getCapability term termColors)
