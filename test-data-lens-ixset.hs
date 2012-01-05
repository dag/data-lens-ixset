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

prop_get_set :: FirstName -> LastName -> Bool
prop_get_set fname lname = getL l (setL l p empty) == p
  where
    l = ixLens fname
    p = Just $ Person fname lname

prop_set_get :: FirstName -> LastName -> Bool
prop_set_get fname lname = setL l (getL l ix) ix == ix
  where
    l  = ixLens fname
    ix = fromList [Person fname lname]

prop_set_set :: FirstName -> LastName -> LastName -> Bool
prop_set_set fname lname1 lname2 =
    setL l p1 (setL l p2 empty) == setL l p1 empty
  where
    l  = ixLens fname
    p1 = Just $ Person fname lname1
    p2 = Just $ Person fname lname2

main = do
  success <- $quickCheckAll
  when (not success) exitFailure
