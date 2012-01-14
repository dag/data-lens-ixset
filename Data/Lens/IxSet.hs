module Data.Lens.IxSet (ixLens) where

import Data.IxSet
import Data.Lens.Common
import Data.Typeable

-- |Focus on a key in an indexed set.
--
-- Given an 'IxSet' of people:
--
-- > people = fromList [ Person (FirstName "Edward A.") (LastName "Kmett")
-- >                   , Person (FirstName "Simon") (LastName "P. Jones")
-- >                   ]
--
-- We can now work with indices using lenses and fix Simon's last name:
--
-- > people' = ixLens (FirstName "Simon") ^%= fmap (lastName ^= LastName "Peyton-Jones") $ people
--
-- Perhaps more commonly you're working with an IxSet from inside a state
-- monad such as with acid-state.  In that case usage is even easier:
--
-- > changeLastName = ixLens (FirstName "Simon") %= fmap (lastName ^= LastName "Peyton-Jones")
--
-- Here's the missing boilerplate:
--
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Data.Data
-- > import Data.IxSet
-- > import Data.Lens
-- > import Data.Lens.Template
-- >
-- > data Person = Person { _firstName :: FirstName
-- >                      , _lastName  :: LastName
-- >                      } deriving (Show, Eq, Ord, Data, Typeable)
-- >
-- > mkLens ''Person
-- >
-- > newtype FirstName = FirstName String
-- >   deriving (Show, Eq, Ord, Data, Typeable)
-- >
-- > newtype LastName = LastName String
-- >   deriving (Show, Eq, Ord, Data, Typeable)
-- >
-- > instance Indexable Person where
-- >   empty = ixSet [ ixGen (Proxy :: Proxy FirstName)
-- >                 , ixGen (Proxy :: Proxy LastName)
-- >                 ]
ixLens :: (Indexable a, Typeable a, Typeable k, Ord a)
       => k -> Lens (IxSet a) (Maybe a)
ixLens k = lens get set
  where
    get          = getOne . getEQ k
    set (Just v) = updateIx k v
    set Nothing  = deleteIx k
