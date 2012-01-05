{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Data.Data
import Data.IxSet
import Data.Lens.Common
import Data.Lens.IxSet
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.All

data Person = Person FirstName LastName
  deriving (Show, Eq, Ord, Data, Typeable)

newtype FirstName = FirstName String
  deriving (Show, Eq, Ord, Data, Typeable, Arbitrary)

newtype LastName = LastName String
  deriving (Show, Eq, Ord, Data, Typeable, Arbitrary)

instance Indexable Person where
  empty = ixSet [ ixGen (Proxy :: Proxy FirstName)
                , ixGen (Proxy :: Proxy LastName)
                ]

instance Arbitrary Person where
  arbitrary = do
    firstname <- arbitrary
    lastname <- arbitrary
    return $ Person firstname lastname

instance (Arbitrary a, Indexable a, Typeable a, Ord a)
      => Arbitrary (IxSet a) where
  arbitrary = do
    items <- arbitrary
    return $ fromList items

prop_get_set :: FirstName -> LastName -> IxSet Person -> Bool
prop_get_set fname lname ix = getL l (setL l p ix) == p
  where
    l = ixLens fname
    p = Just $ Person fname lname

prop_set_get :: FirstName -> IxSet Person -> Bool
prop_set_get fname ix = setL l (getL l ix) ix == ix
  where
    l = ixLens fname

prop_set_set :: FirstName -> LastName -> LastName -> IxSet Person -> Bool
prop_set_set fname lname1 lname2 ix = setL l p1 (setL l p2 ix) == setL l p1 ix
  where
    l  = ixLens fname
    p1 = Just $ Person fname lname1
    p2 = Just $ Person fname lname2

main = do
  success <- $quickCheckAll
  when (not success) exitFailure
