{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Data
import Data.IxSet
import Data.Lens
import Data.Lens.IxSet
import Data.Lens.Template

newtype FirstName = FirstName String
  deriving (Show, Eq, Ord, Data, Typeable)

newtype LastName = LastName String
  deriving (Show, Eq, Ord, Data, Typeable)

data Person = Person { _firstName :: FirstName
                     , _lastName  :: LastName
                     } deriving (Show, Eq, Ord, Data, Typeable)

makeLens ''Person

instance Indexable Person where
  empty = ixSet [ ixGen (Proxy :: Proxy FirstName)
                , ixGen (Proxy :: Proxy LastName)
                ]

people = fromList [ Person (FirstName "Edward A.") (LastName "Kmett")
                  , Person (FirstName "Simon") (LastName "P. Jones")
                  ]

people' = ixLens (FirstName "Simon") ^%= fmap (lastName ^= LastName "Peyton-Jones") $ people
