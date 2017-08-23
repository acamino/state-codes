-- | Main entry point, exposing all that needs to be

module Data.StateCodes
  ( StateCode(..)
  , allNames
  , stateList
  , fromMName
  , fromMText
  , fromName
  , fromText
  , toName
  , toText
  ) where

import           Control.Arrow              ((&&&))
import           Data.Text                  (Text)

import           Data.StateCodes.ISO31662US

-- | List all codes with names

allNames :: [(StateCode, Text)]
allNames = map (id &&& toName) $ enumFrom minBound
