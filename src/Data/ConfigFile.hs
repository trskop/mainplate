-- |
-- Module:      Data.ConfigFile
-- Description: Configuration file
-- Copyright:   (c) 2017-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Configuration file
module Data.ConfigFile
    (
    -- $usageExample

    -- * IsConfigFilePath
      IsConfigFilePath(..)

    -- * HasConfigFilePath
    , HasConfigFilePath(..)
    , getConfigFilePath
    , setConfigFilePath
    , modifyConfigFilePath

    -- * ConfigFile
    , ConfigFile(..)
    , configFile

    -- * YAML
    , parseYamlConfig
    , parseYamlConfigFile
    , parseYamlConfigFilePath
    )
  where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Coerce (coerce)
import Data.Either (Either)
import Data.Eq (Eq)
import Data.Function ((.), id)
import Data.Functor (Functor, (<$>), fmap)
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Kind (Type)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Last(Last), Monoid(mempty, mappend))
import Data.Semigroup (Semigroup((<>)))
import Data.String (IsString(fromString), String)
import Data.Traversable (Traversable, traverse)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml (ParseException, decodeFileEither)
import Dhall (FromDhall, ToDhall)
import System.FilePath (normalise)

import System.FilePath.Parse (parseFilePath)


-- {{{ ConfigFile -------------------------------------------------------------

-- | Optional file path to configuration file.
newtype ConfigFile = ConfigFile {getConfigFile :: Maybe FilePath}
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromDhall, ToDhall)

-- | Smart constructor for 'ConfigFile':
--
-- @
-- \"\" -> 'mempty' -- = 'ConfigFile' 'Nothing'
-- path -> 'ConfigFile' ('Just' ('normalise' path))
-- @
configFile :: FilePath -> ConfigFile
configFile = ConfigFile . \case
    "" -> Nothing
    s  -> Just (normalise s)

-- | Same semantics as @'Semigroup' ('Last' 'FilePath')@.
--
-- >>> configFile "foo" <> configFile "bar"
-- ConfigFile (Just "bar")
instance Semigroup ConfigFile where
    (<>) = coerce ((<>) :: Last FilePath -> Last FilePath -> Last FilePath)

-- | Same semantics as @'Monoid' ('Last' 'FilePath')@.
--
-- >>> mempty :: ConfigFile
-- ConfigFile Nothing
instance Monoid ConfigFile where
    mempty = ConfigFile Nothing
    mappend = (<>)

-- | Defined as @'fromString' = 'configFile'@, see 'configFile' for details.
instance IsString ConfigFile where
    fromString = configFile

-- }}} ConfigFile -------------------------------------------------------------

-- {{{ IsConfigFilePath -------------------------------------------------------

class IsConfigFilePath a where
    -- | Parse 'String' that represents file path to a configuration file.
    -- Value @Left errorMessage@ is returned when parsing fails.
    parseConfigFilePath :: String -> Either String a

instance IsConfigFilePath FilePath where
    parseConfigFilePath = parseFilePath

instance IsConfigFilePath a => IsConfigFilePath (Maybe a) where
    parseConfigFilePath = fmap Just . parseConfigFilePath

instance IsConfigFilePath ConfigFile where
    parseConfigFilePath = fmap configFile . parseConfigFilePath

-- Find a better way:
--
--instance
--    ( Semigroup (ConfigFilePath a)
--    , IsConfigFilePath (ConfigFilePath a)
--    , HasConfigFilePath a
--    ) => IsConfigFilePath (a -> a)
--  where
--    parseConfigFilePath = fmap modify . parseConfigFilePath
--      where
--        modify path = modifyConfigFilePath (<> path)

-- }}} IsConfigFilePath -------------------------------------------------------

-- {{{ HasConfigFilePath ------------------------------------------------------

class IsConfigFilePath (ConfigFilePath a) => HasConfigFilePath a where
    type ConfigFilePath a :: Type

    -- | Lens for accessing value of @'ConfigFilePath' a :: Type@ inside
    -- @a :: Type@.
    configFilePath
        :: (Functor f, ConfigFilePath a ~ configFile)
        => (configFile -> f configFile)
        -> a
        -> f a

instance HasConfigFilePath FilePath where
    type ConfigFilePath FilePath = FilePath
    configFilePath = id

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

-- }}} HasConfigFilePath ------------------------------------------------------

-- {{{ YAML -------------------------------------------------------------------

-- | Parse YAML configuration file(s).
--
-- Type @t :: Type -> Type@ is usually specialised to 'Identity', 'Maybe' or
-- @[]@.
parseYamlConfig
    :: forall a configFile config io t
    .   ( HasConfigFilePath a
        , ConfigFilePath a ~ configFile
        , FromJSON config
        , MonadIO io
        , Traversable t
        )
    => (configFile -> t FilePath)
    -> a
    -> io (t (Either Yaml.ParseException config))
parseYamlConfig toPossibleFilePath =
    traverse decodeFile . toPossibleFilePath . getConfigFilePath
  where
    decodeFile = liftIO . Yaml.decodeFileEither
        :: FilePath -> io (Either Yaml.ParseException config)

-- | Variant of 'parseYamlConfig' specialised to 'ConfigFile'.
parseYamlConfigFile
    ::  ( HasConfigFilePath a
        , ConfigFilePath a ~ ConfigFile
        , FromJSON config
        , MonadIO io
        )
    => a
    -> io (Maybe (Either Yaml.ParseException config))
parseYamlConfigFile = parseYamlConfig getConfigFile

-- | Variant of 'parseYamlConfig' specialised to 'FilePath'.
parseYamlConfigFilePath
    ::  ( HasConfigFilePath a
        , ConfigFilePath a ~ FilePath
        , FromJSON config
        , MonadIO io
        )
    => a
    -> io (Either Yaml.ParseException config)
parseYamlConfigFilePath = fmap runIdentity . parseYamlConfig Identity

-- }}} YAML -------------------------------------------------------------------

-- $usageExample
--
-- @
-- import "Data.ConfigFile"
--     ( 'ConfigFile'
--     , 'HasConfigFilePath'('ConfigFilePath', 'configFilePath')
--     )
--
-- data Options = Options
--     { ...
--     , configFile :: 'ConfigFile'
--     }
--
-- instance 'HasConfigFilePath' Options where
--     type 'ConfigFilePath' Options = 'ConfigFile'
--
--     'configFilePath' =
--         'Control.Lens.lens' configFile '$' \\s b -> s{configFile = b}
--
-- data Config = ...
--
-- instance 'FromJSON' Config where ...
-- instance 'FromJSON' ('Endo' Config) where ...
--
-- parseGlobalConfig
--     :: 'MonadIO' io
--     => 'FilePath'
--     -> io ('Either' 'Yaml.ParseException' Config)
-- parseGlobalConfig = 'parseYamlConfigFilePath'
--
-- parseUserConfig
--     :: 'MonadIO' io
--     => Options
--     -> io ('Either' 'Yaml.ParseException' ('Endo' Config))
-- parseUserConfig =
--     'fmap' ('Data.Maybe.fromMaybe' 'mempty') '.' 'parseYamlConfigFile'
-- @
