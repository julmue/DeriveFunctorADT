{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module DeriveFunctor where

import Prelude (Show, id, (.), (+), ($), Int, Maybe(Just, Nothing))

-- ADT building blocks
fix  ::  (a -> a) -> a
fix  f = let a = f a in a

void :: a
void = fix id

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

-- type level function composition
newtype Compose (g :: * -> *) (f :: * -> *) (a :: *) = Compose (g (f a))
  deriving Show

infixr 9 :.:
type (a :.: b) = Compose a b

-- functors and bifunctors
-- Functor: unary function between categories c1 -> c2
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

instance (Functor f, Functor g) => Functor (g :.: f) where
   fmap f (Compose x) = Compose $ (fmap . fmap) f x

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

-- why is the last position meaningless?
type Option = BiComp (:+:) (Const Unit) Id Void

nothing :: Option a
nothing =  BiComp . L . Const $ Unit

option :: a -> Option a
option = BiComp . R . Id

optionVal :: Option Int
optionVal = option 42

succ :: Int -> Int
succ x = x + 1

test1 :: Option Int
test1 = fmap succ optionVal

test2 :: Option Int
test2 = fmap succ nothing

-- type level composition works
type Composition = (Option :.: [])

composition :: a -> (Option :.: Id) a
composition = Compose . option . Id

compVal :: (Option :.: Id) Int
compVal = composition 42

test3 = fmap succ compVal


-- -----------------------------------------------------------------------------
-- these type (inclusive OR) from primitives

-- simple
-- type These a b = a :+: a :*: b :+: b
-- type These a b = (Id a) :+: ((Id a) :*: (Id b)) :+: (Id b)

-- type These a b = (Id a) :+: (Compose (:+:) ((Id a) :*: (Id b)) (Id b))

type Test a b = (Id a :*: Id b)

-- sinistral :: a -> These a b
-- sinistral = L . Id

-- medial :: a -> b -> These a b
-- medial a b = R . L $ (Id a) :*: (Id b)

-- dextral :: b -> These a b
-- dextral = R . R . Id

-- type These = (:+:)
