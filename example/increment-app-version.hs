{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Main
-- Description: Example application that increments ITERATION number.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Example application that increments ITERATION number of an application
-- version stored in YAML configuration file.
module Main (main)
  where

import Control.Applicative ((<*>))
import Control.Arrow (left)
import Control.Exception (displayException)
import Data.Either (Either(Left))
import Data.Eq (Eq((==)))
import Data.Function (($), (.), on)
import Data.Functor (Functor, (<$>), fmap)
import Data.Monoid (Endo(Endo), (<>), mconcat, mempty)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, listToMaybe, maybe)
import Data.Ord (Ord(compare), Ordering(EQ))
import Data.String (String, fromString)
import Data.Word (Word)
import System.Environment (getArgs)
import System.IO (IO)
import Text.Show (Show, show)

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson (object, withObject)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString (putStr, writeFile)
import Data.ConfigFile
    ( ConfigFile(getConfigFile)
    , HasConfigFilePath(ConfigFilePath, configFilePath)
    , configFile
    , parseYamlConfigFile
    , setConfigFilePath
    )
import Data.Text (Text)
import qualified Data.Text as Text (null, unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
--import qualified Data.Text.IO as Text (putStrLn)
import qualified Data.Yaml as Yaml (encode)
import Mainplate (applySimpleDefaults, runAppWith)

import Data.Version.Class
    ( IsVersion(toSomeString)
    )
import qualified Data.Version.Class as Version
    ( HasIteration(Iteration, iteration)
    , increment
    , toText
    )


-- | @MAJOR.MINOR.ITERATION.BUILD[-CUSTOMER][ GIT_COMMIT]@
data AppVersion = AppVersion
    { major :: Word
    , minor :: Word
    , iteration :: Word
    , build :: Word
    , gitCommit :: Text
    , customer :: Text
    }
  deriving (Show)

instance Eq AppVersion where
    (==) = ((== EQ) .) . compare

instance Ord AppVersion where
    compare = compare `on` \AppVersion{major, minor, iteration, build} ->
        (major, minor, iteration, build)

instance ToJSON AppVersion where
    toJSON AppVersion{..} = Aeson.object
        [ "major" .= major
        , "minor" .= minor
        , "iteration" .= iteration
        , "build" .= build
        , "git_commit" .= emptyIsNull gitCommit
        , "customer" .= emptyIsNull customer
        ]
      where
        emptyIsNull txt = if Text.null txt then Nothing else Just txt

-- TODO: This instance is currently unused. Have a global config with
--       default/initial version number?
instance FromJSON AppVersion where
    parseJSON = Aeson.withObject "AppVersion" $ \o -> AppVersion
        <$> o .: "major"
        <*> o .: "minor"
        <*> o .: "iteration"
        <*> o .: "build"
        <*> nothingIsEmpty (o .:? "git_commit")
        <*> nothingIsEmpty (o .:? "customer")
      where
        nothingIsEmpty = fmap (fromMaybe "")

instance FromJSON (Endo AppVersion) where
    parseJSON = Aeson.withObject "Endo AppVersion" $ \o ->
        mk  <$> o .:? "major"
            <*> o .:? "minor"
            <*> o .:? "iteration"
            <*> o .:? "build"
            <*> o .:? "git_commit"
            <*> o .:? "customer"
      where
        mk j n i b g c = Endo $ \AppVersion{..} -> AppVersion
            { major = fromMaybe major j
            , minor = fromMaybe minor n
            , iteration = fromMaybe iteration i
            , build = fromMaybe build b
            , gitCommit = fromMaybe gitCommit g
            , customer = fromMaybe customer c
            }

instance IsVersion AppVersion where
    toSomeString AppVersion{..} =
        showing major <> "." <> showing minor <> "." <> showing iteration
        <> "." <> showing build
        <> (if Text.null customer then "" else "-" <> unpack customer)
        <> (if Text.null gitCommit then "" else " " <> unpack gitCommit)
      where
        showing = fromString . show
        unpack = fromString . Text.unpack

    -- TODO: toBuilder

instance Version.HasIteration AppVersion where
    type Iteration AppVersion = Word
    iteration f s@AppVersion{iteration} =
        (\i -> s{iteration = i}) <$> f iteration

simpleAppVersion :: Word -> Word -> Word -> AppVersion
simpleAppVersion major minor iteration = AppVersion{..}
  where
    build = 0
    gitCommit = ""
    customer = ""

data Mode a = ModifyVersion ConfigFile a
  deriving (Functor)

type Config = AppVersion

instance HasConfigFilePath (Mode a) where
    type ConfigFilePath (Mode a) = ConfigFile
    configFilePath f (ModifyVersion fp a) = (`ModifyVersion` a) <$> f fp
        -- TODO: This lens could be derived.

renderConfig :: Config -> ByteString
renderConfig v = mconcat
    [ "# DO NOT EDIT THIS FILE BY HAND!\n\n"
    , Yaml.encode v, "\n"
    , "haskell_pvp_version: &haskell_pvp_version\n"
    , "  version: ", version, "\n"
    ]
  where
    version = Text.encodeUtf8 $ Version.toText v{gitCommit="", customer=""}

main :: IO ()
main = runAppWith parseOptions readConfig applyDefaults $ \case
    ModifyVersion file cfg ->
        maybe ByteString.putStr ByteString.writeFile (getConfigFile file)
        . renderConfig $ Version.increment Version.iteration cfg
  where
    parseOptions =
        maybe mempty (Endo . setConfigFilePath . configFile) . listToMaybe
        <$> getArgs

    readConfig :: Mode a -> IO (Either String (Endo Config))
    readConfig = go . parseYamlConfigFile
      where
        go = fmap $ \case
            Nothing -> Left "Configuration file wasn't specified, see `--help`."
            Just r  -> left displayException r

    applyDefaults =
        applySimpleDefaults . ModifyVersion "" $ simpleAppVersion 0 0 0
