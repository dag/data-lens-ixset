module Data.Lens.IxSet (ixLens) where

import Data.Data
import Data.IxSet
import Data.Lens.Common

{- |Focus on a key in an indexed set, much like with 'mapLens'.

Given an 'IxSet' of people:

@
people = 'fromList' [ Person (FirstName \"Edward A.\") (LastName \"Kmett\")
                  , Person (FirstName \"Simon\") (LastName \"P. Jones\")
                  ]
@

We can now work with indices using lenses and fix Simon's last name:

@
people\' = 'ixLens' (FirstName \"Simon\") '^%=' 'fmap' (lastName '^=' LastName \"Peyton-Jones\") '$' people
@

>>> people'
fromList [Person {_firstName = FirstName "Edward A.", _lastName = LastName "Kmett"}
         ,Person {_firstName = FirstName "Simon", _lastName = LastName "Peyton-Jones"}]
>>> (firstName ^$) <$> people' ^. ixLens (LastName "Peyton-Jones")
Just (FirstName "Simon")
>>> ixLens (LastName "Peyton-Jones") ^$ people
Nothing

Perhaps more commonly you're working with an 'IxSet' from inside a state
monad such as @Update@ from the @acid-state@ package.  In that case usage
is even easier:

@
changeLastName = 'ixLens' (FirstName \"Simon\") '%=' 'fmap' (lastName '^=' LastName \"Peyton-Jones\")
@

Here's the missing boilerplate, which also needs the packages @data-lens-fd@ and @data-lens-template@:

@
\{\-\# LANGUAGE DeriveDataTypeable \#\-\}
\{\-\# LANGUAGE TemplateHaskell \#\-\}

import Data.Data
import Data.IxSet
import Data.Lens
import Data.Lens.IxSet
import Data.Lens.Template

newtype FirstName = FirstName 'String'
  deriving ('Show', 'Eq', 'Ord', 'Data', 'Typeable')

newtype LastName = LastName 'String'
  deriving ('Show', 'Eq', 'Ord', 'Data', 'Typeable')

data Person = Person { _firstName :: FirstName
                     , _lastName  :: LastName
                     } deriving ('Show', 'Eq', 'Ord', 'Data', 'Typeable')

makeLens ''Person

instance 'Indexable' Person where
  empty = 'ixSet' [ 'ixGen' ('Proxy' :: 'Proxy' FirstName)
                , 'ixGen' ('Proxy' :: 'Proxy' LastName)
                ]
@

-}

ixLens :: (Indexable a, Typeable a, Typeable k, Ord a)
       => k -> Lens (IxSet a) (Maybe a)
ixLens k = lens get set
  where
    get          = getOne . getEQ k
    set (Just v) = updateIx k v
    set Nothing  = deleteIx k
