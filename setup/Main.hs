{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import System.Directory (listDirectory, copyFile)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Monad (forM_, when, (<=<))
import Data.List (intercalate, stripPrefix)

{-# ANN module "HLint: ignore Redundant multi-way if" #-}

newtype Day = MkDay {unDay :: Int}
  deriving (Enum, Num, Show) via Int

instance Bounded Day where
  minBound = 1
  maxBound = 25

-- Copy day 1 to cover all 25 days
main :: IO ()
main = do
  existing <- existingDays
  if | null existing -> createDirs
     | otherwise -> do
       putStrLn $ "Days " <> intercalate ", " (show <$> existing) <> " already exist."
       putStr "Should I delete them? yes/No"
       response <- getLine
       when (response == "yes") createDirs
  where createDirs = forM_ [minBound + 1 :: Day ..] $ copyFile "day1" . ("day" <>) . show

existingDays :: IO [Int]
existingDays = filter inRange . mapMaybe dayDir <$> listDirectory "."
  where
    dayDir = readMaybe <=< stripPrefix "day"

    inRange n = n > minBound + 1 && n <= maxBound
