{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Data.Vec where

import qualified GHC.TypeLits
import GHC.TypeLits hiding (Nat)

data Nat = Z | S Nat
  deriving (Show, Eq, Ord)

type family FromLit (n :: GHC.TypeLits.Nat) where
  FromLit 0 = Z
  FromLit n = S (FromLit (n-1))

type family Pred (x :: Nat) where
  Pred (S n) = n

data Vec :: Nat -> * -> * where
  VNil :: Vec 'Z a
  (:.) :: a -> Vec n a -> Vec ('S n) a
deriving instance Show a => Show (Vec n a)

infixr 5 :.

v :: Vec (FromLit 12) Int
v = pure 10

vhead :: Vec ('S n) a -> a
vhead (x :. _) = x

vtail :: Vec ('S n) a -> Vec n a
vtail (_ :. xs) = xs

vhead' :: Vec n a -> Maybe a
vhead' VNil = Nothing
vhead' xs@(_ :. _) = Just $ vhead xs

vtail' :: Vec n a -> Maybe (Vec (Pred n) a)
vtail' VNil = Nothing
vtail' xs@(_ :. _) = Just $ vtail xs

vlast :: Vec (S n) a -> a
vlast (x :. VNil) = x
vlast (x :. y :. xs) = vlast (y :. xs)

vinit :: Vec (S n) a -> Vec n a
vinit (x :. VNil) = VNil
vinit (x :. y :. xs) = x :. vinit (y :. xs)

instance Foldable (Vec n) where
  foldr _ acc VNil = acc
  foldr f acc (x :. xs) = f x (foldr f acc xs)

instance Functor (Vec n) where
  fmap _ VNil = VNil
  fmap f (x :. xs) = f x :. fmap f xs

instance Applicative (Vec 'Z) where
  pure = const VNil
  (<*>) _ _ = VNil

instance Applicative (Vec n) => Applicative (Vec ('S n)) where
  pure x = x :. pure x
  (<*>) (f :. fs) (x :. xs) = f x :. (fs <*> xs)

instance Traversable (Vec n) where
  traverse _ VNil = pure VNil
  traverse f (x :. xs) = (:.) <$> f x <*> (traverse f xs)
