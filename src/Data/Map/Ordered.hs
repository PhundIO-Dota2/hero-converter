{-# LANGUAGE
    TypeFamilies
    #-}
{-| This module contians a quick-and-dirty ordered map implementation.
    The API largely mimcs the 'Data.Map.Strict' API, but there are additional guarantees about key order.
-}
module Data.Map.Ordered where

import Data.Semigroup

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import qualified GHC.Exts as Exts (IsList(..))

-- | An ordered map type that remembers insertion order.
-- This is composed of a @'Seq' k@ to store key order, and a @'Map' k a@ to store the key-value mapping.
data OrderedMap k a = OrderedMap !(Seq k) !(Map k a)

-- * Operators

-- | O(log n). Find the value at a key. Calls error when the element can not be found.
(!) :: Ord k => OrderedMap k a -> k -> a
OrderedMap _ m ! k = m M.! k

-- | O(log n). Find the value at a key. Returns @'Nothing'@ when the element can not be found.
(!?), lookup :: Ord k => OrderedMap k a -> k -> Maybe a
OrderedMap _ m !? k = M.lookup k m
lookup = (!?)

-- * Query

-- | O(1). Is the map empty?
null :: OrderedMap k a -> Bool
null (OrderedMap ks _) = Seq.null ks

-- | O(1). Number of elements in the map
size :: OrderedMap k a -> Int
size (OrderedMap ks _) = Seq.length ks

-- | O(log n). Is the key a member of the map?
member :: Ord k => k -> OrderedMap k a -> Bool
member k (OrderedMap _ m)= M.member k m

-- * Construction

-- | O(1). The empty map.
empty :: OrderedMap k a
empty = OrderedMap Seq.empty M.empty

-- | O(1). A map with a single element.
singleton :: k -> a -> OrderedMap k a
singleton k v = OrderedMap (Seq.singleton k) (M.singleton k v)

-- | Build an OrderedMap from a list of (key, value) pairs ina  suitable contained.
fromList :: (Foldable f, Ord k) => f (k, v) -> OrderedMap k v
fromList = foldr (uncurry insertEnd) empty

-- | Build an unordered map from a list of (key, value) pairs in a suitable container.
-- This is essentially just a more general form of 'fromList', and is only used internally.
buildMap :: (Foldable f, Ord k) => f (k, v) -> Map k v
buildMap = foldr (uncurry M.insert) M.empty

-- * Insertion

-- | Insert a new key and value into a map. If the key already exists, it is overwritten. The key is appended to the end.
insertEnd :: Ord k => k -> a -> OrderedMap k a -> OrderedMap k a
insertEnd k v (OrderedMap ks m) = OrderedMap newSeq (M.insert k v m)
  where
    newSeq = Seq.filter (/= k) ks Seq.|> k

-- | Insert a new key and value into a map. If the key already exists, it is overwritten. The key is appended to the start.
insertStart :: Ord k => k -> a -> OrderedMap k a -> OrderedMap k a
insertStart k v (OrderedMap ks m) = OrderedMap newSeq (M.insert k v m)
  where
    newSeq = k Seq.<| Seq.filter (/= k) ks

-- | Functor instance for OrderedMap. This instance just delegates to @'Data.Map.Strict.map'@, so it's O(n).
instance Ord k => Functor (OrderedMap k) where
  fmap f (OrderedMap v m) = OrderedMap v (M.map f m)

-- | Foldable instance for OrderedMap. Entries are traversed in the order in which they appear in the key sequence.
instance Ord k => Foldable (OrderedMap k) where
  foldr f z (OrderedMap v m) = foldr (f . (m M.!) ) z v

-- | Traversable instance for OrderedMap. Entries are traversed in the order in which they appear in the key sequence.
instance Ord k => Traversable (OrderedMap k) where
  traverse f (OrderedMap v m) = OrderedMap v . buildMap <$> traverse (\k -> (,) k <$> f (m M.! k)) v

-- | Semigroup instance for OrderedMap.
-- > OrderedMap lk lm <> OrderedMap rk rm = OrderedMap (lk <> rOnly) (Map.unionWith (<>) lm rm)
--  where @rOnly@ is the sequence consisting of all the keys appearing in @rk@ but not in @lk@.
--  Note that @size (m0 <> m1)@ may be less than @size m0 + size m1@
instance (Ord k, Semigroup a) => Semigroup (OrderedMap k a) where
  OrderedMap lk lm <> OrderedMap rk rm = OrderedMap (lk <> rKeys) (M.unionWith (<>) lm rm)
    where
      -- keys which exist only in the right OrderedMap
      rKeys = Seq.filter (`M.notMember` lm) rk

instance (Ord k, Semigroup a) => Monoid (OrderedMap k a) where
  mempty = empty
  mappend = (<>)

instance (Eq k, Eq a) => Eq (OrderedMap k a) where
  OrderedMap ks0 m0 == OrderedMap ks1 m1 = ks0 == ks1 && m0 == m1

instance Ord k => Exts.IsList (OrderedMap k a) where
  type Item (OrderedMap k a) = (k, a)
  fromList = fromList