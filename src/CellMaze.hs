{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs,
             DataKinds, KindSignatures, TypeOperators #-}

module CellMaze where

import Maze
import OrdEnum

import Data.Array
import GHC.TypeLits

class Cell cell dir where
  isAllowed :: cell -> dir -> Bool

data AllowedDirections (n :: Nat) where
  AD0 :: AllowedDirections 0
  AD :: Bool -> AllowedDirections n -> AllowedDirections (n + 1)

instance (OrdEnum d) => Cell (AllowedDirections n) d where
  isAllowed a d = impl a d first
    where impl :: (OrdEnum d) => AllowedDirections n -> d -> d -> Bool
          impl AD0 d c = False
          impl (AD x a) d c
            | d == c    = x
            | otherwise = impl a d (next c)

data CellMaze cell = CellMaze Width Height (Array Position cell)

makeMaze :: c -> Width -> Height -> CellMaze c
makeMaze cell w h =
  let posMin = Position (0, 0)
      posMax = Position (width w - 1, height h - 1)
  in CellMaze w h $ listArray (posMin, posMax) $ repeat $ cell

instance (Cell cell dir) => Maze (CellMaze cell) Position dir where
  checkMove (CellMaze _ _ cells) p = isAllowed (cells!p)
