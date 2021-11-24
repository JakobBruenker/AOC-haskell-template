{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import AOC.Common
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Map.Lazy qualified as ML
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T

import Data.IORef
import Data.STRef
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (IOVector, STVector)
import Data.Vector.Mutable qualified as V

import Data.Attoparsec.Text as P

{-# ANN module ("HLint: ignore Redundant multi-way if" :: String) #-}

main :: IO ()
main = do
  putStrLn "\nPart 1:"
  putStrLn $ part1 input
  putStrLn $ "\n" <> replicate 80 '-'
  putStrLn "\nPart 2:"
  putStrLn $ part2 input
  putStrLn ""

part1 :: String -> String
part1 = const "part1"

part2 :: String -> String
part2 = const "part2"

whenM :: Monad m => m Bool -> m () -> m ()
whenM mcond action = do
  cond <- mcond
  when cond action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mcond action = do
  cond <- mcond
  unless cond action
