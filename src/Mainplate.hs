{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
-- |
-- Module:      Mainplate
-- Description: Patterns used for application main functions
-- Copyright:   (c) 2017-2026 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- __TL;DR__: Patterns used for application @main@ functions.
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
