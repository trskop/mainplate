{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:      System.Environment.Parser
-- Description: Parse environment variables.
-- Copyright:   (c) 2018-2021 Peter Trško
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
    , eitherToParseEnv

    -- ** Evaluate Environment Parser
    , parseEnv
    , parseEnvIO
    , parseEnvWithIO

    -- ** Parsing Primitives
    , var
    , varWith
    , nonEmptyVar
    , nonEmptyVarWith
    , optionalVar
    , optionalVarWith
    )
  where

import Control.Applicative (Alternative, Applicative, pure)
import Control.Monad (Monad, MonadPlus, (>>=), when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fail (MonadFail(fail))
import Data.Bifunctor (bimap)
import Data.Either (Either, either)
import Data.Function (($), (.))
import Data.Functor (Functor, (<$), (<$>), fmap)
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
import qualified Data.Text as Text (null)

import System.Environment.Variable (EnvVarName, EnvVarValue)


data ParseEnvError
    = ParseEnvError EnvVarName String
    | MissingEnvVarError EnvVarName
    | EmptyEnvVarError EnvVarName
    | ErrorMessage String
    | UnknownError
  deriving stock (Generic, Show)

instance Semigroup ParseEnvError where
    e <> UnknownError = e
    _ <> e            = e

instance Monoid ParseEnvError where
    mempty = UnknownError

newtype ParseEnv context a = ParseEnv
    ( ReaderT
        (HashMap EnvVarName EnvVarValue, context)
        (Except ParseEnvError)
        a
    )
  deriving newtype
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadPlus
    )
  deriving newtype (MonadError ParseEnvError)

instance MonadFail (ParseEnv context) where
    fail = throwError . ErrorMessage

instance MonadReader context (ParseEnv context) where
    local f (ParseEnv (ReaderT g)) = ParseEnv $ ReaderT \(env, context) ->
        g (env, f context)

    reader f = ParseEnv $ ReaderT \(_, context) ->
        pure (f context)

-- | Lookup a required environment value. Fail if environment value is not
-- defined. Environment variable value is allowed to be empty. Use
-- `nonEmptyVar` if you require it to be defined and non-empty.
var :: EnvVarName -> ParseEnv context EnvVarValue
var name = optionalVar name >>= maybe (missingEnvVarError name) pure
  where
    missingEnvVarError = throwError . MissingEnvVarError

-- | Lookup a required environment value. Fail if environment value is not
-- defined or if its value is empty. Use `var` if you want to allow empty
-- values.
nonEmptyVar :: EnvVarName -> ParseEnv context EnvVarValue
nonEmptyVar name = do
    value <- var name
    value <$ when (Text.null value) do
        definedButEmptyError name
  where
    definedButEmptyError = throwError . EmptyEnvVarError

optionalVar :: EnvVarName -> ParseEnv context (Maybe EnvVarValue)
optionalVar name =
    ParseEnv $ ReaderT \(env, _) ->
        pure (HashMap.lookup name env)

varWith
    :: EnvVarName
    -> (EnvVarName -> EnvVarValue -> Either ParseEnvError a)
    -> ParseEnv context a
varWith name f = var name >>= eitherToParseEnv . f name

nonEmptyVarWith
    :: EnvVarName
    -> (EnvVarName -> EnvVarValue -> Either ParseEnvError a)
    -> ParseEnv context a
nonEmptyVarWith name f = nonEmptyVar name >>= eitherToParseEnv . f name

optionalVarWith
    :: EnvVarName
    -> (EnvVarName -> Maybe EnvVarValue -> Either ParseEnvError a)
    -> ParseEnv context a
optionalVarWith name f = optionalVar name >>= eitherToParseEnv . f name

eitherToParseEnv :: Either ParseEnvError a -> ParseEnv context a
eitherToParseEnv = either throwError pure

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
