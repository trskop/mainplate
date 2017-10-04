{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:      Data.ApplicationVersion
-- Description: TODO
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO
module Data.Version.Class
--  (
--  )
  where

import Data.Bool (Bool)
import Data.Functor (Functor, (<$>))
import Data.Int (Int)
import Data.Version (Version(Version))
--import Data.Word (Word)

--import Data.Text (Text)
import qualified Data.SemVer as Semantic (Version)
import qualified Data.SemVer as Semantic.Version
    ( isDevelopment
    , isPublic
    , major
    , minor
    , patch
    )


--data family AppVersion (t :: k)

class HasMajor a where
    type Major a :: *
    major :: (Major a ~ major, Functor f) => (major -> f major) -> a -> f a

instance HasMajor Version where
    type Major Version = (Int, Int)
    major f (Version ver tags) = case ver of
        []           -> set [] <$> f (0,  0)
        [a1]         -> set [] <$> f (a1, 0)
        a1 : a2 : vs -> set vs <$> f (a1, a2)
      where
        set vs (a1, a2) = Version (a1 : a2 : vs) tags

instance HasMajor Semantic.Version where
    type Major Semantic.Version = Int
    major = Semantic.Version.major

class HasMinor a where
    type Minor a :: *
    minor :: (Minor a ~ minor, Functor f) => (minor -> f minor) -> a -> f a

instance HasMinor Version where
    type Minor Version = Int
    minor f (Version ver tags) = case ver of
        []               -> set 0  0  [] <$> f 0
        [a1]             -> set a1 0  [] <$> f 0
        [a1, a2]         -> set a1 a2 [] <$> f 0
        a1 : a2 : i : vs -> set a1 a2 vs <$> f i
      where
        set a1 a2 vs i = Version (a1 : a2 : i : vs) tags

instance HasMinor Semantic.Version where
    type Minor Semantic.Version = Int
    minor = Semantic.Version.minor

class HasPatch a where
    type Patch a :: *
    patch :: (Patch a ~ patch, Functor f) => (patch -> f patch) -> a -> f a

instance HasPatch Version where
    type Patch Version = Int
    patch f (Version ver tags) = case ver of
        []                   -> set 0  0  0 [] <$> f 0
        [a1]                 -> set a1 0  0 [] <$> f 0
        [a1, a2]             -> set a1 a2 0 [] <$> f 0
        [a1, a2, i]          -> set a1 a2 i [] <$> f 0
        a1 : a2 : i : p : vs -> set a1 a2 i vs <$> f p
      where
        set a1 a2 i vs p = Version (a1 : a2 : i : p : vs) tags

instance HasPatch Semantic.Version where
    type Patch Semantic.Version = Int
    patch = Semantic.Version.patch

class HasIteration a where
    type Iteration a :: *
    iteration
        :: (Iteration a ~ iteration, Functor f)
        => (iteration -> f iteration)
        -> a -> f a



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

class HasBuild a where
    type Build a :: *
    build :: (Build a ~ build, Functor f) => (build -> f build) -> a -> f a

class HasGitCommit a where
    type GitCommit a :: *
    gitCommit
        :: (GitCommit a ~ commit, Functor f)
        => (commit -> f commit)
        -> a -> f a

class HasRelease a where
    type Release a :: *
    release
        :: (Release a ~ release, Functor f)
        => (release -> f release)
        -> a -> f a

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
