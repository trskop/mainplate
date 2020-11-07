{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      Data.Output
-- Description: Data type representing application (normal) output.
-- Copyright:   (c) 2017-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Data type representin application (normal) output.
module Data.Output
    (
    -- * IsOutput
      IsOutput(..)

    -- * HasOutput
    , HasOutput(..)
    , getOutput
    , setOutput
    , modifyOutput

    -- * OutputFile
    , OutputFile(..)

    -- * Handle
    , OutputHandle(..)

    -- ** OutputStdoutOrFile
    , StdoutOnly
    , OutputStdoutOrFile
    , pattern OutputStdoutOnly

    -- ** OutputStdoutOrStderrOrFile
    , StdoutOrStderr
    , OutputStdoutOrStderrOrFile
    , pattern OutputStdout
    , pattern OutputStderr
    )
  where

import Data.Bifunctor (Bifunctor(bimap, first, second))
import Data.Either (Either(Right))
import Data.Eq (Eq)
import Data.Function ((.), id)
import Data.Functor (Functor, (<$>), fmap)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Kind (Type)
import Data.Maybe (Maybe(Just))
import Data.Semigroup (Option(Option))
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import Dhall (FromDhall, ToDhall)
import System.FilePath.Parse (parseFilePath)


newtype OutputFile = OutputFile {outputFile :: FilePath}
  deriving stock (Eq, Generic, Show)
  deriving newtype (IsOutput)
  deriving newtype (FromDhall, ToDhall)

-- {{{ OutputHandle -----------------------------------------------------------

data StdoutOnly = StdoutOnly
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

data StdoutOrStderr = Stdout | Stderr
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

data OutputHandle handle a
    = OutputHandle handle
    | OutputNotHandle a
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromDhall, ToDhall)

instance Functor (OutputHandle handle) where
    fmap f = \case
        OutputHandle handle -> OutputHandle handle
        OutputNotHandle a   -> OutputNotHandle (f a)

instance Bifunctor OutputHandle where
    bimap f g = \case
        OutputHandle    a -> OutputHandle (f a)
        OutputNotHandle b -> OutputNotHandle (g b)

    first f = \case
        OutputHandle    a -> OutputHandle (f a)
        OutputNotHandle b -> OutputNotHandle b

    second f = \case
        OutputHandle    a -> OutputHandle a
        OutputNotHandle b -> OutputNotHandle (f b)

-- }}} OutputHandle -----------------------------------------------------------

-- {{{ OutputStdoutOrFile -----------------------------------------------------

-- | Output is either a file or a stdout.
type OutputStdoutOrFile = OutputHandle StdoutOnly OutputFile

type OutputStdoutOrStderrOrFile = OutputHandle StdoutOrStderr OutputFile

pattern OutputStdoutOnly :: OutputHandle StdoutOnly a
pattern OutputStdoutOnly = OutputHandle StdoutOnly

pattern OutputStdout :: OutputHandle StdoutOrStderr a
pattern OutputStdout = OutputHandle Stdout

pattern OutputStderr :: OutputHandle StdoutOrStderr a
pattern OutputStderr = OutputHandle Stderr

-- }}} OutputStdoutOrFile -----------------------------------------------------

-- {{{ IsOutput ---------------------------------------------------------------

class IsOutput a where
    parseOutput :: String -> Either String a

instance IsOutput FilePath where
    parseOutput = parseFilePath

instance IsOutput a => IsOutput (Maybe a) where
    parseOutput = fmap Just . parseOutput

instance IsOutput a => IsOutput (Option a) where
    parseOutput = fmap Option . parseOutput

-- |
-- @
-- \"-\" -> 'Right' 'OutputStdoutOnly'
-- str -> 'OutputFile' '<$>' 'parseOutput' str
-- @
instance IsOutput (OutputHandle StdoutOnly OutputFile) where
    parseOutput = \case
        "-" -> Right OutputStdoutOnly
        s   -> OutputNotHandle . OutputFile <$> parseOutput s

-- Find a better way:
--
--instance (IsOutput (Output a), HasOutput a) => IsOutput (a -> a) where
--    parseOutput = fmap setOutput . parseOutput

-- }}} IsOutput ---------------------------------------------------------------

-- {{{ HasOutput --------------------------------------------------------------

class IsOutput (Output a) => HasOutput a where
    type Output a :: Type
    output :: (Functor f, Output a ~ out) => (out -> f out) -> a -> f a

instance HasOutput OutputFile where
    type Output OutputFile = OutputFile
    output = id

instance HasOutput (OutputHandle StdoutOnly OutputFile) where
    type Output (OutputHandle StdoutOnly OutputFile) =
        OutputHandle StdoutOnly OutputFile

    output = id

getOutput :: (Output a ~ output, HasOutput a) => a -> output
getOutput s = getConst (output Const s)

setOutput :: (Output a ~ output, HasOutput a) => output -> a -> a
setOutput o s = runIdentity (output (\_ -> Identity o) s)

modifyOutput
    :: (Output a ~ output, HasOutput a)
    => (output -> output)
    -> a
    -> a
modifyOutput f s = runIdentity (output (Identity . f) s)

-- }}} HasOutput --------------------------------------------------------------
