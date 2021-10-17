-- |
-- Module:      System.FilePath.Parse
-- Description: Parse FilePath from a String.
-- Copyright:   (c) 2017-2021 Peter Trško
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

import Data.Bool (not, otherwise)
import Data.Either (Either(Left, Right))
import Data.String (IsString, String)
import System.IO (FilePath)

import System.FilePath
    ( dropTrailingPathSeparator
    , hasTrailingPathSeparator
    , isValid
    , normalise
    )


-- | Parse 'String' as a 'FilePath' pointing to a file. What this function does
-- is the following:
--
-- * Checks that the file path is not empty (@\"\"@) or contains invalid
--   characters.
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
parseFilePath possibleFilePath
  | not (isValid possibleFilePath) =
        Left (validationErrorToString InvalidFilePath)

  | hasTrailingPathSeparator normalisedFilePath =
        Left (validationErrorToString UnexpectedDirectoryPath)

  | otherwise =
        Right normalisedFilePath
  where
    normalisedFilePath = normalise possibleFilePath

-- | Parse 'String' as a 'FilePath' pointing to a directory. What the function
-- does is the following:
--
-- * Checks that file path is not empty (@\"\"@) or contains invalid
--   characters.
--
-- * Removes trailing path separator.
--
-- * Normalises the file path.
--
-- Anything beyond that would require IO, which is intentionally not used to
-- allow use cases in pure code.
parseDirectoryPath :: String -> Either String FilePath
parseDirectoryPath possibleFilePath
  | not (isValid possibleFilePath) =
        Left (validationErrorToString InvalidFilePath)

  | otherwise =
        Right (dropTrailingPathSeparator (normalise possibleFilePath))

data ValidationError
    = InvalidFilePath
    | UnexpectedDirectoryPath

validationErrorToString :: IsString s => ValidationError -> s
validationErrorToString = \case
    InvalidFilePath ->
        "Invalid file path, empty path or contains invalid character."
    UnexpectedDirectoryPath ->
        "Expected path to a file, not a directory."
