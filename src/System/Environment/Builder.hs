-- |
-- Module:      System.Environment.Builder
-- Description: Builder for environment (list of environment variables)
-- Copyright:   (c) 2018-2026 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Builder for environment (list of environment variables).
module System.Environment.Builder
    (
    -- * Environment Builder
      EnvBuilder(..)
    , getEnv
    )
  where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.Function (($), (.), const)
import Data.Functor ((<$>), fmap)
import Data.String (fromString)
import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty))
import GHC.Generics (Generic)
import System.Environment (getEnvironment)

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap (fromList)

import System.Environment.Variable (EnvVarName, EnvVarValue)


newtype EnvBuilder context = EnvBuilder
    { buildEnv :: context -> HashMap EnvVarName EnvVarValue
    }
  deriving stock Generic

instance Semigroup (EnvBuilder context) where
    (<>) :: EnvBuilder context -> EnvBuilder context -> EnvBuilder context
    EnvBuilder f <> EnvBuilder g = EnvBuilder $ \context ->
        f context <> g context

instance Monoid (EnvBuilder context) where
    mempty :: EnvBuilder context
    mempty = EnvBuilder (const mempty)

getEnv :: MonadIO io => io (EnvBuilder context)
getEnv = EnvBuilder . const . mkHashMap <$> liftIO getEnvironment
  where
    mkHashMap = HashMap.fromList . fmap (bimap fromString fromString)
