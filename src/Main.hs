{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# LANGUAGE DerivingVia         #-}
module Main where

import           Prelude

import           Control.Applicative (Alternative (..), liftA2)
import           Data.Proxy          (Proxy (..))
import           Data.Semigroup      (First (..), Sum(..), Product(..), Semigroup)
import           GHC.TypeLits
import           Test.QuickCheck


-- * Introduction
-- What can we do with DerivingVia?

-- | Here, we have @First@ Semigroup.
-- This always returns first element when composed by (<>) oeprator.
--
-- >>> getFirst $ First 3 <> First 5
-- 3
-- >>> getFirst $ First Nothing <> First (Just 5) <> First (Just 9)
-- Nothing

-- | Define @Status@ sum type.
-- This derives @Semigroup@ *via* @First Status@.

data Status = Green | Yellow | Red
  deriving (Eq, Show)
  deriving Semigroup via (First Status)

-- | This data always returns the first element when composed.
--
-- >>> Green <> Yellow <> Red
-- Green
-- >>> Yellow <> Red <> Green
-- Yellow


-- * The motivation

-- | Why do we need this extension?
-- To see that, we might need to review what we have been suffering from.
--
-- Let's think of IO-like applicative.

newtype IO' a = IO' a
  deriving (Show)

instance Functor IO' where
  fmap f (IO' x) = IO' (f x)

instance Applicative IO' where
  pure x = IO' x
  (IO' f) <*> (IO' x) = IO' (f x)

-- | When defining an instance of @Semigroup@,
-- it will naturally unwrap, apply the operator and rewrap.

instance Semigroup a => Semigroup (IO' a) where
  (<>) = liftA2 (<>)

-- | Like...
-- >>> IO' Yellow <> IO' Green
-- IO' Yellow


-- | Simillarly, we have ST-like applicative.

newtype ST' a = ST' a
  deriving (Show)

instance Functor ST' where
  fmap f (ST' x) = ST' (f x)

instance Applicative ST' where
  pure x = ST' x
  (ST' f) <*> (ST' x) = ST' (f x)

-- | Again we have @Semigroup@ instance as well.

instance Semigroup a => Semigroup (ST' a) where
  (<>) = liftA2 (<>)


-- | Now we feel annoyed because of the duplicated code.
-- Can we make @Semigroup@ instances at once?
-- Let's see...
--
-- instance (Applicative f, Semigroup a) => Semigroup (f a) where
--   (<>) = liftA2 (<>)

-- | Unfortunately, this does not work well.
-- Because in this way, the instance is applied on all @Applicative@ @Semigroup@
-- instances without any exception.


-- | What is the matter of it?
-- Here, we have List-like applicative.

data List a = Nil | Cons a (List a)
  deriving (Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = fs' $ fmap f xs
    where
      fs' Nil           = fs <*> xs
      fs' (Cons x' xs') = Cons x' (fs' xs')

-- | Note that this @Applicatie@ instance works as a product of functions and
-- elements.
--
-- >>> Cons (*2) (Cons (+1) Nil) <*> Cons 3 (Cons 4 Nil)
-- Cons 6 (Cons 8 (Cons 4 (Cons 5 Nil)))

-- | This is just like an ordinary list as...
--
-- >>> [(*2), (+1)] <*> [3, 4] :: [Int]
-- [6,8,4,5]



-- * Now what happens if we have a generalized version of @Semigroup@ instance?
-- It would work like:

-- instance Semigroup a => Semigroup (List a)
--   where (<>) = liftA2 (<>)

--- |
-- >>> Cons Yellow (Cons Red Nil) <> Cons Green (Cons Yellow Nil)
-- Cons Yellow (Cons Yellow (Cons Red (Cons Red Nil)))

-- * What a tragedy!
-- This is not what we want.
-- What we what is this:

instance Semigroup (List a) where
  Nil <> xs = xs
  xs <> Nil = xs
  Cons x xs <> ys = Cons x $ xs <> ys

-- | Let's see if this works as we expect:
--
-- >>> Cons Yellow (Cons Red Nil) <> Cons Green (Cons Yellow Nil)
-- Cons Yellow (Cons Red (Cons Green (Cons Yellow Nil)))

-- | Yeah, this is what we want!

-- | Like this way, we cannot write @Applicative@ @Semigroup@ instances even
-- thogh the implementations are the same like among IO and ST.
-- This is because once an instances defiened, there is no way to revert it.
--
-- As so, we have to write instances explicity one by one.
-- Well, we had to.
--
-- Now we have DerivingVia.

-- * Application of DerivingVia

-- | OK, now it looks a time to go into the world of DerivingVia.
-- To generalize @Applicative@ @Semigroup@, first we define an @Ap@ wrapper.

newtype Ap f a = Ap (f a)

-- | And instance of @Semigroup@.

instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  Ap f <> Ap g = Ap $ liftA2 (<>) f g


-- | With this new approach, we don't need to write @Applicative@ @Semigroup@
-- by hand anymore.
-- We can apply it with `via` keyword.

newtype IO'' a = IO'' a
  deriving (Show)
  deriving Semigroup via (Ap IO'' a)

-- | This requires @Applicative@ instance defined independently.
-- And when we get it, we have @Semigroup@ instance for free!

instance Functor IO'' where
  fmap f (IO'' x) = IO'' (f x)

instance Applicative IO'' where
  pure x = IO'' x
  (IO'' f) <*> (IO'' x) = IO'' (f x)


-- | It is easy to see why this works.
-- Due to the use of `newtype`, @Ap IO'' a@ has the same internal representation
-- as @IO'' a@.
-- Or @Ap IO'' a@ and @IO'' a@ are representationally equal.

-- | Another example of @Semigroup@ whose instance is taken from @Alternative@.

newtype Alt f a = Alt (f a)

instance Alternative f => Semigroup (Alt f a) where
  Alt f <> Alt g = Alt (f <|> g)

-- | Here we have a new version of List.
-- This is just a newtype wrapper of @List a@ data type we already got.

newtype List' a = List' (List a)
  deriving (Show)
  deriving (Functor, Applicative) via (List)
  deriving Semigroup via (Alt List' a)

-- | This works when we get an instance of @Applicative@ which can be written:

instance Alternative List' where
  empty = List' Nil
  List' Nil <|> x = x
  x <|> _ = x

-- | Now we got @Semigroup@ instance for free!
--
-- >>> List' Nil <> List' (Cons Red Nil) <> List' (Cons Green Nil)
-- List' (Cons Red Nil)

-- | Yes, this is appending in an alternative way!


-- * Case study of QuickCheck
--
-- QuickCheck provides newtype adapters so that users can put some term level
-- constraints on generated values.
-- @NonNegative@ is one of those modifiers.

-- newtype NonNegative a = NonNegative { getNonNegative :: a }
-- instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a)

-- | When we have data type which represents non-negative length we can go like:

newtype Length = Length Int
  deriving Show
  deriving Arbitrary via NonNegative Int

-- |
-- prop> \(Length i) -> i >= 0

-- | QuickCheck also provides @Large@ modifier, whose definition is:

-- newtype Large a = Large { getLarge :: a }
-- instance (Integral a, Bounded a) => Arbitrary (Large a)

-- | Like with this modifier, we can nicely compose modifiers:

newtype Huge = Huge Int
  deriving Show
  deriving Arbitrary via (NonNegative (Large Int))

-- |
-- prop> \(Huge i) -> i >= 0

-- | This double-wrapped deriving via still works.
-- It shares the same representation between @Huge Int@ and
-- @NonNegative (Large Int)@, thus be able to reuse the @Arbitrary@ instance from
-- @NonNegative Large@.


-- | It also available to define an @Arbitrary@ instance from an arbitrary
-- function.

newtype BoundedEnum a = BoundedEnum a

instance (Bounded a, Enum a) => Arbitrary (BoundedEnum a) where
  arbitrary = BoundedEnum <$> arbitraryBoundedEnum

data Weekday = Mo | Tu | We | Th | Fr | Sa | Su
  deriving (Eq, Show, Bounded, Enum)
  deriving Arbitrary via BoundedEnum Weekday

nextWorkday :: Weekday -> Weekday
nextWorkday Mo = Tu
nextWorkday Tu = We
nextWorkday We = Th
nextWorkday Th = Fr
nextWorkday Fr = Mo
nextWorkday Sa = Mo
nextWorkday Su = Mo

-- |
-- prop> \(wd :: Weekday) -> fromEnum (nextWorkday wd) == fromEnum wd + 1 || nextWorkday wd == Mo


-- | Another example of using type level constraints.

newtype Between (l :: Nat) (u :: Nat) = Between Integer

instance (KnownNat l, KnownNat u) => Arbitrary (Between l u) where
  arbitrary = Between <$> choose (natVal @l Proxy, natVal @u Proxy)

newtype Year = Year Integer
  deriving Show
  deriving Arbitrary via (Between 1900 2100)

-- |
-- prop> \(Year year) -> 1900 <= year && year <= 2100


-- * Example of Pair

newtype Pair = Pair (Int, Int)
  deriving Show
  deriving Semigroup via (Sum Int, Product Int)

-- |
-- >>> Pair (3, 4) <> Pair (5, 6)
-- Pair (8,24)



main :: IO ()
main = putStrLn "Deriving GoodCodingLife via Haskell!"
