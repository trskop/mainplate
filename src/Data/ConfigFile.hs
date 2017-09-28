{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.ConfigFile
-- Description: Configuration file
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Configuration file
module Data.ConfigFile
--  (
--  )
  where

import Control.Applicative (Applicative)
import Data.Coerce (coerce)
import Data.Either (Either)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor (Functor, (<$>), fmap)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Last(Last), Monoid(mempty, mappend))
import Data.Semigroup (Semigroup((<>)))
import Data.String (String)
import Data.Traversable (Traversable, traverse)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import System.FilePath.Parse (parseFilePath)


newtype ConfigFile = ConfigFile {getConfigFile :: Maybe FilePath}
  deriving (Eq, Generic, Show)

instance Semigroup ConfigFile where
    (<>) = coerce ((<>) :: Last FilePath -> Last FilePath -> Last FilePath)

instance Monoid ConfigFile where
    mempty = ConfigFile Nothing
    mappend = (<>)

class IsConfigFilePath a where
    parseConfigFilePath :: String -> Either String a

instance IsConfigFilePath FilePath where
    -- TODO: File paths on windows have much more restrictions than this.
    parseConfigFilePath = parseFilePath

instance IsConfigFilePath a => IsConfigFilePath (Maybe a) where
    parseConfigFilePath = fmap Just . parseConfigFilePath

instance IsConfigFilePath ConfigFile where
    parseConfigFilePath = fmap ConfigFile . parseConfigFilePath

instance
    ( IsConfigFilePath (ConfigFilePath a)
    , HasConfigFilePath a
    ) => IsConfigFilePath (a -> a)
  where
    parseConfigFilePath = fmap setConfigFilePath . parseConfigFilePath

class IsConfigFilePath (ConfigFilePath a) => HasConfigFilePath a where
    type ConfigFilePath a :: *

    configFilePath
        :: (Functor f, ConfigFilePath a ~ configFile)
        => (configFile -> f configFile)
        -> a
        -> f a

instance HasConfigFilePath ConfigFile where
    type ConfigFilePath ConfigFile = Maybe FilePath
    configFilePath f (ConfigFile fp) = ConfigFile <$> f fp

getConfigFilePath
    :: (ConfigFilePath a ~ configFile, HasConfigFilePath a)
    => a
    -> configFile
getConfigFilePath s = getConst (configFilePath Const s)

setConfigFilePath
    :: (ConfigFilePath a ~ configFile, HasConfigFilePath a)
    => configFile
    -> a -> a
setConfigFilePath a s = runIdentity (configFilePath (\_ -> Identity a) s)

modifyConfigFilePath
    :: (ConfigFilePath a ~ configFile, HasConfigFilePath a)
    => (configFile -> configFile)
    -> a -> a
modifyConfigFilePath f s = runIdentity (configFilePath (coerce f) s)

parseConfigWith
    ::  ( ConfigFilePath a ~ t FilePath
        , HasConfigFilePath a
        , Applicative f
        , Traversable t
        )
    => (FilePath -> f b)
    -- ^ Read and parse config file.
    -> a
    -> f (t b)
parseConfigWith readAndParse = traverse readAndParse . getConfigFilePath
