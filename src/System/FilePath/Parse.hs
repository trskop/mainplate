{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      System.FilePath.Parse
-- Description: Parse FilePath from a String.
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Parse 'FilePath' from a 'String'.
module System.FilePath.Parse
    ( parseFilePath
    )
  where

import Data.String (String)
import Data.Either (Either(Left, Right))
import System.IO (FilePath)

import System.FilePath (normalise, splitFileName)


-- | Parse 'String' as a 'FilePath' pointing to a file.
parseFilePath :: String -> Either String FilePath
parseFilePath = \case
    "" -> Left "Empty file path is not allowed."
    s  -> case splitFileName s' of
        (_, "") -> Left "Expected path to a file, not directory."
        _       -> Right s'
      where
        s' = normalise s

