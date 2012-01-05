module Data.Lens.IxSet (ixLens) where

import Data.IxSet
import Data.Lens.Common
import Data.Typeable

ixLens :: (Indexable a, Typeable a, Typeable k, Ord a)
       => k -> Lens (IxSet a) (Maybe a)
ixLens k = lens get set
  where
    get          = getOne . getEQ k
    set (Just v) = updateIx k v
    set Nothing  = deleteIx k
