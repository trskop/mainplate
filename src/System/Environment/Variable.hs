{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      System.Environment.Variable
-- Description: Representation of environment variables
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Representation of environment variables.
module System.Environment.Variable
    ( EnvVarName
    , EnvVarValue
    , EnvironmentVariable(..)
    , fromTuple
    , toTuple
    )
  where

import Data.Eq (Eq)
import Data.Ord (Ord)
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)

import Data.Text (Text)
import qualified Dhall (Inject, Interpret)


type EnvVarName = Text
type EnvVarValue = Text

data EnvironmentVariable = EnvironmentVariable
    { name :: EnvVarName
    , value :: EnvVarValue
    }
  deriving (Eq, Generic, Ord, Read, Show)

instance Dhall.Inject EnvironmentVariable
instance Dhall.Interpret EnvironmentVariable

fromTuple :: (EnvVarName, EnvVarValue) -> EnvironmentVariable
fromTuple (name, value) = EnvironmentVariable{name, value}

toTuple :: EnvironmentVariable -> (EnvVarName, EnvVarValue)
toTuple EnvironmentVariable{name, value} = (name, value)
