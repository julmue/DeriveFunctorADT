{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}

module DeriveFunctor where

import Prelude (Show, id, (.), (+), ($), Int)

-- ADT building blocks

-- zero
data Void = Void Void deriving Show

-- one
data Unit = Unit deriving Show

-- product type constructor
infixr 3 :*:
data (a :*: b) = a :*: b deriving Show

-- sum type constructor
infixr 2 :+:
data (a :+: b) = L a | R b deriving Show

-- constant functor
data Const a b = Const a deriving Show

-- identity in CAT
data Id a = Id a deriving Show

-- functors and bifunctors
-- Functor: unary function between categories c1 -> c2
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

instance Functor Id where
  fmap f (Id a) = Id (f a)

-- Bifunctor: binary structure preserving function between categories (c1 * c2) -> c3
class Bifunctor (f :: * -> * -> *) where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
  bimap f1 f2 = second f2 . first f1
  first :: (a -> c) -> f a b -> f c b
  first f = bimap f id
  second :: (b -> d) -> f a b -> f a d
  second f = bimap id f

instance Bifunctor (:*:) where
  bimap f1 f2 (a :*: b) = f1 a :*: f2 b

instance Bifunctor (:+:) where
  bimap f1 _ (L a) = L (f1 a)
  bimap _ f2 (R b) = R (f2 b)

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

instance {-# OVERLAPPING #-} Bifunctor f => Functor (f a) where
  fmap = second

-- -----------------------------------------------------------------------------
-- composition of bifuctors
newtype BiComp bf fu gu a b = BiComp (bf (fu a) (gu b)) deriving Show

-- composition of Bifunctor with functor
instance (Bifunctor bf, Functor fu, Functor gu) =>
  Bifunctor (BiComp bf fu gu) where
  bimap f1 f2 (BiComp f) = BiComp (bimap (fmap f1) (fmap f2) f)

-- -----------------------------------------------------------------------------
-- option type from primitives

-- type Maybe a = Const Unit a :+: Id a
type Maybe a = BiComp (:+:) (Const Unit) Id a a

nothing =  BiComp . L . Const $ Unit

just :: a -> Maybe a
just = BiComp . R . Id

justVal :: Maybe Int
justVal = just 42

succ :: Int -> Int
succ x = x + 1

test1 :: Maybe Int
test1 = fmap succ justVal

test2 :: Maybe Int
test2 = fmap succ nothing
