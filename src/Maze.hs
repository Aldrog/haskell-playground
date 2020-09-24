{-# LANGUAGE MultiParamTypeClasses #-}

module Maze where

import Data.Ix

class Maze maze position dir where
  checkMove :: maze -> position -> dir -> Bool

newtype Width = Width { width :: Int }
newtype Height = Height { height :: Int }
newtype Position = Position (Int, Int)
  deriving (Eq, Ord, Show, Ix)
