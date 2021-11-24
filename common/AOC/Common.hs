{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module AOC.Common
  ( module AOC.Common
  , module Data.Functor
  , module Control.Applicative
  , module Control.Monad
  , module Control.Monad.Reader
  , module Control.Monad.ST
  , module Control.Monad.IO.Class
  , module Control.Monad.Except
  , module Data.Monoid
  , module Data.Maybe
  , module Data.Either
  , module Data.Char
  , module Data.These
  , module Data.Foldable
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Data.Profunctor
  , module Data.Ord
  , module Data.Coerce
  , module Data.Data
  , module Data.Data.Lens
  , module Data.Generics.Labels
  , module Data.Kind
  , module Control.Category
  , module Data.Function
  , module Data.List
  , module Data.Word
  , module Data.Bool
  , module Data.Bits
  , module Data.Void
  , module Numeric.Lens
  , module Text.Read
  , module System.Random
  , module GHC.Generics
  , module GHC.Stack

  , module Prelude
  , module Data.Boolean.Overload

  , module Control.Lens
  , module Data.List.Split
  , module GHC.IO
  , module Debug.Trace
  ) where

import Data.Maybe
import Data.Either
import Data.Char
import Data.These
import Data.Functor
import Data.Profunctor hiding (WrappedArrow(..))
import Data.Ord
import Data.Coerce
import Data.Data
import Data.Data.Lens
import Data.Generics.Labels
import Data.Kind
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.Except
import Data.Monoid
import Data.Foldable
import Data.Bifunctor
import Data.Bitraversable
import Control.Category hiding ((.), id)
import Data.Function
import Data.List hiding (uncons)
import Data.Word
import Data.Bool hiding ((&&), (||), not)
import Data.Bits
import Data.Void
import Numeric.Lens
import Text.Read hiding (lift, get, step)
import System.Random hiding (split)
import GHC.Generics (Generic)
import GHC.Stack

import Prelude hiding ((&&), (||), not)
import Data.Boolean.Overload ((&&), (||), not)

import Control.Lens
import Data.List.Split

import GHC.IO (unsafePerformIO)

import Debug.Trace

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f x = f x
applyWhen False _ x = x

applyN :: Int -> (a -> a) -> a -> a
applyN n f = applyWhen (n > 0) $ applyN (n - 1) f . f

(.:) :: (b -> c) -> (a -> d -> b) -> a -> d -> c
f .: g = (f .) . g

-- Not pretty, but potentially slightly more convenient than having it be in IO
-- and reasonably safe
{-# NOINLINE input #-}
input :: String
input = unsafePerformIO $ readFile "input/input.txt"

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (<<$>>)

justifyLeft :: Int -> a -> [a] -> [a]
justifyLeft n x xs = replicate (n - length xs) x <> xs

justifyRight :: Int -> a -> [a] -> [a]
justifyRight n x xs = xs <> replicate (n - length xs) x

loeb :: Functor f => f (f a -> a) -> f a
loeb = moeb fmap

moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f x = go where go = f ($ go) x

whenM :: Monad m => m Bool -> m () -> m ()
whenM mcond action = do
  cond <- mcond
  when cond action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mcond action = do
  cond <- mcond
  unless cond action
