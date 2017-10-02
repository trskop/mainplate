{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
-- |
-- Module:      Mainplate
-- Description: TODO
-- Copyright:   (c) 2017 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO
module Mainplate
    (
    -- * Most Common\/Generic Patterns
      module Mainplate.Core

    -- * Extensible Applications
    , module Mainplate.Extensible
    )
  where

import Mainplate.Core
import Mainplate.Extensible
