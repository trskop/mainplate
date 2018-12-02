{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:      System.Environment.Parser
-- Description: Parse environment variables.
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Parse environment variables.
module System.Environment.Parser
    (
    -- * Environment Parser
      ParseEnv(..)
    , ParseEnvError(..)

    -- ** Evaluate Environment Parser
    , parseEnv
    , parseEnvIO
    , parseEnvWithIO

    -- ** Parsing Primitives
    , var
    , optionalVar
    )
  where

import Control.Applicative (Alternative, Applicative, pure)
import Control.Monad (Monad, MonadPlus, (>>=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fail (MonadFail(fail))
import Data.Bifunctor (bimap)
import Data.Either (Either, either)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$>), fmap)
import Data.Functor.Identity (Identity(Identity))
import Data.String (String, fromString)
import Data.Semigroup (Semigroup((<>)))
import Data.Maybe (Maybe, maybe)
import Data.Monoid (Monoid(mempty))
import GHC.Generics (Generic)
import System.Environment (getEnvironment)
import Text.Show (Show)

import Control.Monad.Reader (MonadReader(local, reader), ReaderT(ReaderT))
import Control.Monad.Except (Except, ExceptT(ExceptT), MonadError, throwError)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)

import System.Environment.Variable (EnvVarName, EnvVarValue)


data ParseEnvError
    = ParseEnvError EnvVarName String
    | MissingEnvVarError EnvVarName
    | ErrorMessage String
    | UnknownError
  deriving (Generic, Show)

instance Semigroup ParseEnvError where
    e <> UnknownError = e
    _ <> e            = e

instance Monoid ParseEnvError where
    mempty = UnknownError

newtype ParseEnv context a = ParseEnv
    { parseEnv'
        :: ReaderT
            (HashMap EnvVarName EnvVarValue, context)
            (Except ParseEnvError)
            a
    }
  deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadError ParseEnvError
    , MonadPlus
    )

instance MonadFail (ParseEnv context) where
    fail = throwError . ErrorMessage

instance MonadReader context (ParseEnv context) where
    local f (ParseEnv (ReaderT g)) = ParseEnv . ReaderT $ \(env, context) ->
        g (env, f context)

    reader f = ParseEnv . ReaderT $ \(_, context) ->
        pure (f context)

var :: EnvVarName -> ParseEnv context EnvVarValue
var name = optionalVar name >>= maybe (missingEnvVarError name) pure
  where
    missingEnvVarError = throwError . MissingEnvVarError

optionalVar :: EnvVarName -> ParseEnv context (Maybe EnvVarValue)
optionalVar name =
    ParseEnv . ReaderT $ \(env, _) ->
        pure (HashMap.lookup name env)

-- | Parse environment variables using provided parser.
parseEnv
    :: context
    -> HashMap EnvVarName EnvVarValue
    -> ParseEnv context a
    -> Either ParseEnvError a
parseEnv context env (ParseEnv (ReaderT f)) =
    case f (env, context) of
        ExceptT (Identity r) -> r

-- | Generalised version of 'parseEnvIO' that is not bound to 'ParseEnv'
-- implementation.
parseEnvWithIO
    :: MonadIO io
    => (HashMap EnvVarName EnvVarValue -> parser -> Either err a)
    -- ^ Evaluate parser.
    -> (forall void. err -> io void)
    -- ^ Handle parsing error as an IO exception.
    -> parser
    -> io a
parseEnvWithIO f onError parser = do
    env <- mkHashMap <$> liftIO getEnvironment
    either onError pure (f env parser)
  where
    mkHashMap :: [(String, String)] -> HashMap EnvVarName EnvVarValue
    mkHashMap = HashMap.fromList . fmap (bimap fromString fromString)

-- | Variant of 'parseEnv' that populates the environment by reading process
-- environment variables.
parseEnvIO
    :: MonadIO io
    => context
    -- ^ Prefix of Command Wrapper environment variables.
    -> (forall void. ParseEnvError -> io void)
    -- ^ Handle parsing error as an IO exception.
    -> ParseEnv context a
    -> io a
parseEnvIO context = parseEnvWithIO (parseEnv context)
