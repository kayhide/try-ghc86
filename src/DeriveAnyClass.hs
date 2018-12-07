{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DeriveAnyClass where

import Prelude

-- * Case 1 - anyclass vs newtype

-- | Type class which has a function with default implementation.
class C a where
  format :: a -> String
  format _ = "default"

-- | An instance of @C@.
instance C Int where
  format = ("int: "<>) . show

instance C Bool where
  format = ("bool: " <>) . show

-- | Wrapper type of @C@ which takes a default function.
--
-- >>> format $ A (3 :: Int)
-- "default"
-- >>> format $ A True
-- "default"
newtype A a = A a
  -- deriving C   -- Warning!
  deriving anyclass C


-- | Wrapper type of @C@ which derives a function from unwrapped.
--
-- >>> format $ N (3 :: Int)
-- "int: 3"
-- >>> format $ N True
-- "bool: True"
newtype N a = N a
  deriving newtype C


-- * Case 2 - stock vs newtype

-- |
-- >>> show $ ShowS (3 :: Int)
-- "ShowS 3"
newtype ShowS a = ShowS a
  deriving stock Show

-- |
-- >>> show $ ShowN (3 :: Int)
-- "3"
newtype ShowN a = ShowN a
  deriving newtype Show

