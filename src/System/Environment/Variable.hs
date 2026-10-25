-- |
-- Module:      System.Environment.Variable
-- Description: Representation of environment variables
-- Copyright:   (c) 2018-2026 Peter Trško
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
import Dhall (FromDhall, ToDhall)


type EnvVarName = Text
type EnvVarValue = Text

data EnvironmentVariable = EnvironmentVariable
    { name :: EnvVarName
    , value :: EnvVarValue
    }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving anyclass (FromDhall, ToDhall)

fromTuple :: (EnvVarName, EnvVarValue) -> EnvironmentVariable
fromTuple (name, value) = EnvironmentVariable{name, value}

toTuple :: EnvironmentVariable -> (EnvVarName, EnvVarValue)
toTuple EnvironmentVariable{name, value} = (name, value)
