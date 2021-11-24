{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module AOC.Common
  ( module AOC.Common
  , module Data.Functor
  , module Control.Applicative
  , module Control.Monad
  , module Data.Monoid
  , module Data.Maybe
  , module Data.Either
  , module Data.Char
  , module Data.These
  , module Data.Foldable
  , module Data.Bifunctor
  , module Data.Bitraversable
  , module Control.Category
  , module Data.Function
  , module Data.List
  , module Text.Read
  , module System.Random
  
  , module Prelude
  , module Data.Boolean.Overload

  , module Control.Lens
  , module Control.Lens.TH
  , module Data.List.Split
  , module GHC.IO
  ) where

import Data.Maybe
import Data.Either
import Data.Char
import Data.These
import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Bifunctor
import Data.Bitraversable
import Control.Category hiding ((.), id)
import Data.Function
import Data.List hiding (uncons)
import Text.Read
import System.Random hiding (split)

import Prelude hiding ((&&), (||), not)
import Data.Boolean.Overload ((&&), (||), not)

import Control.Lens
import Control.Lens.TH
import Data.List.Split

import GHC.IO (unsafePerformIO)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f x = f x
applyWhen False _ x = x

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
