{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.ApplicationVersion
-- Description: Various type classes for use with version numbers represented
--              as data types.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Each package registry \/ application \/ software product has its own special
-- needs when it comes to versioning. This module is trying to be as agnostic
-- as possible, so that it can be reused in different contexts.
module Data.Version.Class
    (
    -- $versions

    -- * IsVersion
      IsVersion(..)
    , toText
    , toLazyText
    , toTextWith
    , toLazyTextWith

    -- * HasVersion

    , HasVersion(..)
    , getVersion
    , setVersion
    , modifyVersion

    -- * Version Components

    -- ** Major Version Number
    , HasMajor(..)
    , getMajor
    , setMajor
    , modifyMajor

    -- ** Minor Version Number
    , HasMinor(..)
    , getMinor
    , setMinor
    , modifyMinor

    -- ** Patch
    , HasPatch(..)
    , getPatch
    , setPatch
    , modifyPatch

    -- ** Iteration
    , HasIteration(..)
    , getIteration
    , setIteration
    , modifyIteration

    -- *** Sprint
    , HasSprint
    , sprint
    , getSprint
    , setSprint
    , modifySprint

    -- ** Build
    , HasBuild(..)
    , getBuild
    , setBuild
    , modifyBuild

    -- ** Release
    , HasRelease(..)
    , getRelease
    , setRelease
    , modifyRelease

    -- ** GitCommit
    , HasGitCommit(..)
    , getGitCommit
    , setGitCommit
    , modifyGitCommit

    -- * Predicates
    , CanBeDevelopment(..)
    , CanBePublic(..)
    , CanBeProduction(..)
    , CanBeRelease(..)
    , CanBeReleaseCandidate(..)
    , CanBeStable(..)
    , CanBeLts(..)
    )
  where

import Data.Bool (Bool)
import Data.Coerce (coerce)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor (Functor, (<$>))
import Data.Functor.Const (Const(Const, getConst))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Int (Int)
import Data.Monoid (Monoid)
import Data.Ord (Ord)
import Data.String (IsString, fromString)
import qualified Data.Version as HaskellPvp (Version(Version), showVersion)
--import Data.Word (Word)

import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder (toLazyTextWith)
import qualified Data.SemVer as Semantic (Version)
import qualified Data.SemVer as Semantic.Version
    ( isDevelopment
    , isPublic
    , major
    , minor
    , patch
    , toBuilder
    , toString
    )


-- {{{ HasMajor ---------------------------------------------------------------

class HasMajor a where
    type Major a :: *
    major :: (Major a ~ major, Functor f) => (major -> f major) -> a -> f a

instance HasMajor HaskellPvp.Version where
    type Major HaskellPvp.Version = (Int, Int)
    major f (HaskellPvp.Version ver tags) = case ver of
        []           -> set [] <$> f (0,  0)
        [a1]         -> set [] <$> f (a1, 0)
        a1 : a2 : vs -> set vs <$> f (a1, a2)
      where
        set vs (a1, a2) = HaskellPvp.Version (a1 : a2 : vs) tags

instance HasMajor Semantic.Version where
    type Major Semantic.Version = Int
    major = Semantic.Version.major

getMajor :: (HasMajor a, Major a ~ major) => a -> major
getMajor s = getConst (major coerce s)

setMajor :: (HasMajor a, Major a ~ major) => major -> a -> a
setMajor a s = runIdentity (major (\_ -> coerce a) s)

modifyMajor :: (HasMajor a, Major a ~ major) => (major -> major) -> a -> a
modifyMajor f s = runIdentity (major (coerce f) s)

-- }}} HasMajor ---------------------------------------------------------------

-- {{{ HasMinor ---------------------------------------------------------------

class HasMinor a where
    type Minor a :: *
    minor :: (Minor a ~ minor, Functor f) => (minor -> f minor) -> a -> f a

instance HasMinor HaskellPvp.Version where
    type Minor HaskellPvp.Version = Int
    minor f (HaskellPvp.Version ver tags) = case ver of
        []               -> set 0  0  [] <$> f 0
        [a1]             -> set a1 0  [] <$> f 0
        [a1, a2]         -> set a1 a2 [] <$> f 0
        a1 : a2 : i : vs -> set a1 a2 vs <$> f i
      where
        set a1 a2 vs i = HaskellPvp.Version (a1 : a2 : i : vs) tags

instance HasMinor Semantic.Version where
    type Minor Semantic.Version = Int
    minor = Semantic.Version.minor

getMinor :: (HasMinor a, Minor a ~ minor) => a -> minor
getMinor s = getConst (minor coerce s)

setMinor :: (HasMinor a, Minor a ~ minor) => minor -> a -> a
setMinor a s = runIdentity (minor (\_ -> coerce a) s)

modifyMinor :: (HasMinor a, Minor a ~ minor) => (minor -> minor) -> a -> a
modifyMinor f s = runIdentity (minor (coerce f) s)

-- }}} HasMinor ---------------------------------------------------------------

-- {{{ HasPatch ---------------------------------------------------------------

class HasPatch a where
    type Patch a :: *
    patch :: (Patch a ~ patch, Functor f) => (patch -> f patch) -> a -> f a

instance HasPatch HaskellPvp.Version where
    type Patch HaskellPvp.Version = Int
    patch f (HaskellPvp.Version ver tags) = case ver of
        []                   -> set 0  0  0 [] <$> f 0
        [a1]                 -> set a1 0  0 [] <$> f 0
        [a1, a2]             -> set a1 a2 0 [] <$> f 0
        [a1, a2, i]          -> set a1 a2 i [] <$> f 0
        a1 : a2 : i : p : vs -> set a1 a2 i vs <$> f p
      where
        set a1 a2 i vs p = HaskellPvp.Version (a1 : a2 : i : p : vs) tags

instance HasPatch Semantic.Version where
    type Patch Semantic.Version = Int
    patch = Semantic.Version.patch

getPatch :: (HasPatch a, Patch a ~ patch) => a -> patch
getPatch s = getConst (patch coerce s)

setPatch :: (HasPatch a, Patch a ~ patch) => patch -> a -> a
setPatch a s = runIdentity (patch (\_ -> coerce a) s)

modifyPatch :: (HasPatch a, Patch a ~ patch) => (patch -> patch) -> a -> a
modifyPatch f s = runIdentity (patch (coerce f) s)

-- }}} HasPatch ---------------------------------------------------------------

-- {{{ HasIteration -----------------------------------------------------------

class HasIteration a where
    type Iteration a :: *
    iteration
        :: (Iteration a ~ iteration, Functor f)
        => (iteration -> f iteration)
        -> a -> f a

getIteration :: (HasIteration a, Iteration a ~ iteration) => a -> iteration
getIteration s = getConst (iteration coerce s)

setIteration
    :: (HasIteration a, Iteration a ~ iteration)
    => iteration
    -> a -> a
setIteration a s = runIdentity (iteration (\_ -> coerce a) s)

modifyIteration
    :: (HasIteration a, Iteration a ~ iteration)
    => (iteration -> iteration)
    -> a -> a
modifyIteration f s = runIdentity (iteration (coerce f) s)

-- }}} HasIteration -----------------------------------------------------------

-- {{{ HasSprint --------------------------------------------------------------

-- | Sprint (from Scrum methodology) is an iteration, but iteration is not
-- necessarily a Sprint.
--
-- \"All sprints are iterations but not all iterations are sprints. Iteration
-- is a common term in iterative and incremental development (IID). Scrum is
-- one specialized flavor of IID so it makes sense to specialize the
-- terminology as well.\"
--
-- Source: <https://stackoverflow.com/a/1227406>
class HasIteration a => HasSprint a

sprint
    :: (HasSprint a, Iteration a ~ sprint, Functor f)
    => (sprint -> f sprint)
    -> a -> f a
sprint = iteration

getSprint :: (HasSprint a, Iteration a ~ sprint) => a -> sprint
getSprint = getIteration

setSprint :: (HasSprint a, Iteration a ~ sprint) => sprint -> a -> a
setSprint = setIteration

modifySprint
    :: (HasSprint a, Iteration a ~ sprint)
    => (sprint -> sprint)
    -> a -> a
modifySprint = modifyIteration

-- }}} HasSprint --------------------------------------------------------------

-- {{{ HasBuild ---------------------------------------------------------------

class HasBuild a where
    type Build a :: *
    build :: (Build a ~ build, Functor f) => (build -> f build) -> a -> f a

getBuild :: (HasBuild a, Build a ~ build) => a -> build
getBuild s = getConst (build coerce s)

setBuild :: (HasBuild a, Build a ~ build) => build -> a -> a
setBuild a s = runIdentity (build (\_ -> coerce a) s)

modifyBuild :: (HasBuild a, Build a ~ build) => (build -> build) -> a -> a
modifyBuild f s = runIdentity (build (coerce f) s)

-- }}} HasBuild ---------------------------------------------------------------

-- {{{ HasGitCommit -----------------------------------------------------------

class HasGitCommit a where
    type GitCommit a :: *
    gitCommit
        :: (GitCommit a ~ commit, Functor f)
        => (commit -> f commit)
        -> a -> f a

getGitCommit :: (HasGitCommit a, GitCommit a ~ commit) => a -> commit
getGitCommit s = getConst (gitCommit coerce s)

setGitCommit :: (HasGitCommit a, GitCommit a ~ commit) => commit -> a -> a
setGitCommit a s = runIdentity (gitCommit (\_ -> coerce a) s)

modifyGitCommit
    :: (HasGitCommit a, GitCommit a ~ commit)
    => (commit -> commit)
    -> a -> a
modifyGitCommit f s = runIdentity (gitCommit (coerce f) s)

-- }}} HasGitCommit -----------------------------------------------------------

-- {{{ HasRelease -------------------------------------------------------------

class HasRelease a where
    type Release a :: *
    release
        :: (Release a ~ release, Functor f)
        => (release -> f release)
        -> a -> f a

getRelease :: (HasRelease a, Release a ~ release) => a -> release
getRelease s = getConst (release coerce s)

setRelease :: (HasRelease a, Release a ~ release) => release -> a -> a
setRelease a s = runIdentity (release (\_ -> coerce a) s)

modifyRelease
    :: (HasRelease a, Release a ~ release)
    => (release -> release)
    -> a -> a
modifyRelease f s = runIdentity (release (coerce f) s)

-- }}} HasRelease -------------------------------------------------------------

-- {{{ Predicates -------------------------------------------------------------

class CanBeDevelopment a where
    -- TODO: Use "Contravariant f".
    isDevelopment :: a -> Bool

instance CanBeDevelopment Semantic.Version where
    isDevelopment = Semantic.Version.isDevelopment

class CanBeRelease a where
    isRelease :: a -> Bool

class CanBeReleaseCandidate a where
    isReleaseCandidate :: a -> Bool

class CanBePublic a where
    isPublic :: a -> Bool

instance CanBePublic Semantic.Version where
    isPublic = Semantic.Version.isPublic

class CanBeProduction a where
    isProduction :: a -> Bool

-- | Applications, or even operating systems, may provide so called
-- <https://en.wikipedia.org/wiki/Long-term_support LTS (Long Term Support)>
-- releases. This class provides 'isLts', which returns
-- 'Data.Bool.True' for versions of such software.
--
-- For example:
--
-- * Jenkins LTS releases have version @MAJOR.ITERATION.PATCH@, while normal
--   releases have just @MAJOR.ITERATION@. For more information see
--   <https://jenkins.io/download/lts/>.
-- * Ubuntu LTS releases are released biannually in April and their version
--   numbers start with @YY.04@ where @YY@ is two-digit year. For more
--   information see e.g. <https://en.wikipedia.org/wiki/Ubuntu_version_history>
--   and <https://wiki.ubuntu.com/LTS>.
class CanBeLts a where
    -- | Predicate that checks if version of type @a :: *@ is a LTS (Long Term
    -- Support) version.
    isLts :: a -> Bool

-- | Applications, or even operating systems, may provide so called _stable_
-- releases. This class provides 'isStable', which returns 'Data.Bool.True' for
-- versions of such software.
--
-- For example:
--
-- * Debian has stable releases, and those have version numbers. Other versions
--   (testing and unstable) have only code names. For more information see
--   <https://www.debian.org/releases/>.
class CanBeStable a where
    -- | Predicate that checks if version of type @a :: *@ is a stable version.
    isStable :: a -> Bool

-- }}} Predicates -------------------------------------------------------------

-- {{{ IsVersion --------------------------------------------------------------

-- | Instances for 'Eq' and 'Ord' must respect the semantics of specific
-- version type.
class (Eq a, Ord a) => IsVersion a where
    {-# MINIMAL toSomeString #-}

    -- | Show version as a any string type.
    toSomeString :: (IsString s, Monoid s) => a -> s

    -- | Convert version into a 'Text.Builder'. Can be used when version is
    -- inserted into longer text. See also:
    --
    -- * 'toText'
    -- * 'toLazyText'
    -- * 'toTextWith'
    -- * 'toLazyTextWith'
    --
    -- Default implementation is 'toSomeString'.
    toBuilder :: a -> Text.Builder
    toBuilder = toSomeString

    -- TODO:
    -- fromString :: String -> Either String a
    -- fromLazyText :: Text -> Either String a

instance IsVersion HaskellPvp.Version where
    toSomeString = fromString . HaskellPvp.showVersion

instance IsVersion Semantic.Version where
    toSomeString = fromString . Semantic.Version.toString
    toBuilder = Semantic.Version.toBuilder

-- | Show version as (strict) 'Text'.
--
-- This function uses 'toLazyText', notes mentioned in its documentation apply.
toText :: IsVersion a => a -> Text
toText = Lazy.Text.toStrict . toLazyText

-- | Show version as (lazy) 'Lazy.Text'.
--
-- Be aware that this implementation uses 'toLazyTextWith' with a small buffer
-- size optimised for common version formats. Use 'toLazyTextWith' to use
-- different buffer size.
toLazyText :: IsVersion a => a -> Lazy.Text
toLazyText = toLazyTextWith 24 -- Value taken from semver package.

-- | Show version as (strict) 'Text'.
toTextWith :: IsVersion a => a -> Text
toTextWith = Lazy.Text.toStrict . toLazyText

-- | Show version as (lazy) 'Lazy.Text'.
toLazyTextWith :: IsVersion a => Int -> a -> Lazy.Text
toLazyTextWith buffSize = Text.Builder.toLazyTextWith buffSize . toBuilder

-- }}} IsVersion --------------------------------------------------------------

-- {{{ HasVersion -------------------------------------------------------------

class IsVersion (Version a) => HasVersion a where
    type Version a :: *

    -- | Lens for accessing @version :: *@ value stored in type @a :: *@.
    version
        :: (Version a ~ version, Functor f)
        => (version -> f version)
        -> a -> f a

-- | @'version' = 'id'@
instance HasVersion HaskellPvp.Version where
    type Version HaskellPvp.Version = HaskellPvp.Version
    version = id

-- | @'version' = 'id'@
instance HasVersion Semantic.Version where
    type Version Semantic.Version = Semantic.Version
    version = id

getVersion :: (Version a ~ version, HasVersion a) => a -> version
getVersion s = getConst (version coerce s)

setVersion :: (Version a ~ version, HasVersion a) => version -> a -> a
setVersion a s = runIdentity (version (\_ -> coerce a) s)

modifyVersion
    :: (Version a ~ version, HasVersion a)
    => (version -> version)
    -> a -> a
modifyVersion f s = runIdentity (version (coerce f) s)

-- }}} HasVersion -------------------------------------------------------------

-- $versions
--
-- Application version numbers may have a complicated structure. In this
-- section we will describe some existing versioning schemes so that the
-- purpose of this module becomes clearer.
--
-- One should point out that version numbers aren't actually numbers, but
-- strings with special format where the most prominent part is usually series
-- of numbers separated by dots.
--
-- == Semantic Versioning
--
-- This versioning scheme takes the form of @MAJOR.MINOR.PATCH@, its probably
-- one of the most widely used versioning schemes out there.
-- /Semantic Versioning 2.0/ specification is available on
-- <http://semver.org/ semver.org>.
--
-- Haskell package that provides data type compatible with Semantic Versioning
-- specification is <http://hackage.haskell.org/package/semver semver>. It
-- provides "Data.SemVer" module with 'Semantic.Version' data type.
--
-- == Haskell Package Version Policy (PVP)
--
-- Haskell, actually Cabal, packages use @MAJOR1.MAJOR2.MINOR.PATCH@ scheme.
-- PVP specification is available on <https://pvp.haskell.org/ pvp.haskell.org>.
--
-- Data type that represents PVP versions is called 'HaskellPvp.Version' and it
-- can be found in the <http://hackage.haskell.org/package/base base> package
-- in its "Data.Version" module.
--
-- == Jenkins Versioning Scheme
--
-- For normal releases Jenkins uses @MAJOR.ITERATION@ versioning scheme, where
-- ITERATION is week long.
--
-- Jenkins LTS releases have @MAJOR.ITERATION.PATCH@ versioning scheme, For
-- more information see <https://jenkins.io/download/lts/>.
--
-- == OpenSSL Versioning Scheme
--
-- TODO: Describe OpenSSL versioning to show that not every version component
-- must be a number.
--
-- == Deb and Rpm Packages
--
-- TODO: Describe Deb and Rpm package versioning scheme, which is a great way
-- to illustrate complexity of versioning.
