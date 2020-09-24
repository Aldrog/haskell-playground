module Direction where

import OrdEnum

data Direction = Left | Right | Up | Down
  deriving (Eq)

instance OrdEnum Direction where
  first = Direction.Left

  next Direction.Left  = Direction.Right
  next Direction.Right = Direction.Up
  next Direction.Up    = Direction.Down
  next Direction.Down  = Direction.Left
