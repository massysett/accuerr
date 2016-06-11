{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

-- | Provides 'Accuerr', a type similar to 'Either' but where the
-- @Applicative@ instance accumulates error values.  This module is
-- based on work in the @validation@ package, which is available at
--
-- <https://hackage.haskell.org/package/validation>
--
-- The main differences between the @validation@ package and this one:
--
-- * @validation@ has more data types, many of which overlap with
-- those available in other packages.  This package sticks with the
-- 'Accuerr' type only.
--
-- * This package works with GHC 8; as of 2016-06-11, @validation@ does not.
--
-- * 'Accuerr' has fewer typeclass instances than its corresponding type
-- in @validation@.

module Accuerr where

import Data.Bifunctor (Bifunctor(bimap))
import Data.Bifoldable (Bifoldable(bifoldMap))
import Data.Bitraversable (Bitraversable(bitraverse))
import Data.Semigroup (Semigroup((<>)))
import qualified Control.Lens as Lens

-- | A type similar to 'Either' but the 'Applicative' instance
-- accumulates error values.  Unlike 'Either', there is no 'Monad'
-- instance, because there is no '>>=' such that 'Control.Monad.ap'
-- equals '<*>'.
--
-- For the 'Applicative' instance to work, your error type must be a
-- 'Semigroup', such as a list or a 'Data.List.NonEmpty.NonEmpty'.
--
-- ==== __Examples__
--
-- >>> import Text.Read
-- >>> :{
--        let readInt x = case readMaybe x of
--              Nothing -> AccFailure [x]
--              Just a -> AccSuccess a
--                where _types = a :: Int
--     :}
--
-- >>> (+) <$> readInt "3" <*> readInt "4"
-- AccSuccess 7
-- >>> (+) <$> readInt "x3" <*> readInt "4"
-- AccFailure ["x3"]
-- >>> (+) <$> readInt "x3" <*> readInt "x4"
-- AccFailure ["x3","x4"]
-- >>> (,,) <$> readInt "3" <*> readInt "4" <*> readInt "x5"
-- AccFailure ["x5"]
-- >>> sequenceA [AccSuccess 3, AccSuccess 4]
-- AccSuccess [3,4]
-- >>> sequenceA [AccSuccess 3, AccSuccess 4, AccFailure ['c'], AccFailure ['a']]
-- AccFailure "ca"
data Accuerr e a
  = AccFailure e
  | AccSuccess a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

Lens.makePrisms ''Accuerr

instance Semigroup e => Applicative (Accuerr e) where
  pure = AccSuccess
  AccFailure e1 <*> AccFailure e2 = AccFailure (e1 <> e2)
  AccFailure e1 <*> AccSuccess _ = AccFailure e1
  AccSuccess _ <*> AccFailure e2 = AccFailure e2
  AccSuccess f <*> AccSuccess a = AccSuccess (f a)

instance Bifunctor Accuerr where
  bimap fab fcd x = case x of
    AccFailure a -> AccFailure (fab a)
    AccSuccess c -> AccSuccess (fcd c)

instance Bifoldable Accuerr where
  bifoldMap f _ (AccFailure a) = f a
  bifoldMap _ g (AccSuccess b) = g b

instance Bitraversable Accuerr where
  bitraverse f _ (AccFailure x) = AccFailure <$> f x
  bitraverse _ g (AccSuccess y) = AccSuccess <$> g y

-- | Case analysis for the 'Accuerr' type.  Like 'either'.
accuerr :: (a -> c) -> (b -> c) -> Accuerr a b -> c
accuerr f _ (AccFailure a) = f a
accuerr _ f (AccSuccess a) = f a

accuerrToEither :: Accuerr e a -> Either e a
accuerrToEither (AccFailure e) = Left e
accuerrToEither (AccSuccess a) = Right a

eitherToAccuerr :: Either e a -> Accuerr e a
eitherToAccuerr (Left e) = AccFailure e
eitherToAccuerr (Right a) = AccSuccess a

isoAccuerrEither :: Lens.Iso' (Accuerr e a) (Either e a)
isoAccuerrEither = Lens.iso accuerrToEither eitherToAccuerr

isoEitherAccuerr :: Lens.Iso' (Either e a) (Accuerr e a)
isoEitherAccuerr = Lens.iso eitherToAccuerr accuerrToEither

instance Lens.Swapped Accuerr where
  swapped = Lens.iso to to
    where
      to (AccFailure e) = AccSuccess e
      to (AccSuccess a) = AccFailure a
