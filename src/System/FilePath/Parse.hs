-- |
-- Module:      System.FilePath.Parse
-- Description: Parse FilePath from a String.
-- Copyright:   (c) 2017-2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Parse 'FilePath' from a 'String'.
module System.FilePath.Parse
    ( parseFilePath
    , parseDirectoryPath
    )
  where

import Data.String (String)
import Data.Either (Either(Left, Right))
import System.IO (FilePath)

import System.FilePath (dropTrailingPathSeparator, normalise, splitFileName)


-- | Parse 'String' as a 'FilePath' pointing to a file. What this function does
-- is the following:
--
-- * Checks that the file path is not empty (@\"\"@).
--
-- * Checks for cases when the file path is obviously not a directory, in other
--   words, it checks that it doesn't have a trailing path separator character
--   at the end.
--
-- * Normalises the file path.
--
-- Anything beyond that would require IO, which is intentionally not used to
-- allow use cases in pure code.
parseFilePath :: String -> Either String FilePath
parseFilePath s = \case
    "" ->   Left "Empty file path is not allowed."
    s | hasTrailingPathSeparator s' ->
            Left "Expected path to a file, not a directory."
      | otherwise ->
            Right s'
      where
        s' = normalise s

-- | Parse 'String' as a 'FilePath' pointing to a directory. What the function
-- does is the following:
--
-- * Checks that file path is not empty (@\"\"@).
--
-- * Removes trailing path separator.
--
-- * Normalises the file path.
--
-- Anything beyond that would require IO, which is intentionally not used to
-- allow use cases in pure code.
parseDirectoryPath :: String -> Either String FilePath
parseDirectoryPath = \case
    "" -> Left "Empty file path is not allowed."
    s  -> Right (dropTrailingPathSeparator (normalise s))
