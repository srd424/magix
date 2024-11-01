-- |
-- Module      :  Magix.Hash
-- Description :  Create hashes of Magix configurations
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 31 06:47:58 2024.
module Magix.Hash
  ( getMagixHash,
  )
where

import Crypto.Hash.SHA256 (finalize, init, update)
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (..))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Paths_magix (version)
import Prelude hiding (init)

getMagixHash :: FilePath -> ByteString -> ByteString
getMagixHash nixpkgsPath scriptContents =
  finalize $
    foldl'
      update
      init
      [ encodeUtf8 $ pack nixpkgsPath,
        encodeUtf8 $ pack $ show version,
        scriptContents
      ]
