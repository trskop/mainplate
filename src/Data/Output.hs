{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.Output
-- Description: Data type representing application (normal) output.
-- Copyright:   (c) 2017 Peter Trško
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

    -- * Stdout or File
    , OutputStdoutOrFile
    )
  where

import Data.Either (Either(Right))
import Data.Eq (Eq)
import Data.Function ((.), id)
import Data.Functor (Functor, (<$>), fmap)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.String (String)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import System.FilePath.Parse (parseFilePath)


-- {{{ OutputStdoutOrFile -----------------------------------------------------

-- | Output is either a file or a stdout.
data OutputStdoutOrFile
    = OutputStdout
    | OutputFile FilePath
  deriving (Eq, Generic, Show)

-- }}} OutputStdoutOrFile -----------------------------------------------------

-- {{{ IsOutput ---------------------------------------------------------------

class IsOutput a where
    parseOutput :: String -> Either String a

instance IsOutput FilePath where
    parseOutput = parseFilePath

-- |
-- @
-- \"-\" -> 'Right' 'OutputStdout'
-- str -> 'OutputFile' '<$>' 'parseOutput' str
-- @
instance IsOutput OutputStdoutOrFile where
    parseOutput = \case
        "-" -> Right OutputStdout
        s   -> OutputFile <$> parseOutput s

instance (IsOutput (Output a), HasOutput a) => IsOutput (a -> a) where
    parseOutput = fmap setOutput . parseOutput

-- }}} IsOutput ---------------------------------------------------------------

-- {{{ HasOutput --------------------------------------------------------------

class IsOutput (Output a) => HasOutput a where
    type Output a :: *
    output :: (Functor f, Output a ~ out) => (out -> f out) -> a -> f a

instance HasOutput OutputStdoutOrFile where
    type Output OutputStdoutOrFile = OutputStdoutOrFile
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
