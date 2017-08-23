{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Data.StateCodesSpec where

import           Data.Aeson
import qualified Data.Aeson            as A
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Data.StateCodes

instance Arbitrary StateCode where
  arbitrary = elements [minBound ..]


spec :: Spec
spec = do
  prop "fromName . toName" $ forAll arbitrary $ \code ->
    (fromName . toName) code == code

  prop "fromText . toText" $ forAll arbitrary $ \code ->
    (fromText . toText) code == code

  prop "fromJSON . toJSON" $ forAll (arbitrary :: Gen StateCode)$ \code ->
    (fromJSON . toJSON) code == A.Success code
