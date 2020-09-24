{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs, DataKinds, KindSignatures, TypeOperators #-}

module Playground where

import GHC.TypeLits

class MazeCell cell dir where
  isAllowed :: cell -> dir -> Bool

class (Eq e) => OrdEnum e where
  first :: e
  next :: e -> e

data AllowedDirections (n :: Nat) where
  AD0 :: AllowedDirections 0
  AD :: Bool -> AllowedDirections n -> AllowedDirections (1 + n)

isAllowedImpl :: (OrdEnum d) => AllowedDirections n -> d -> d -> Bool
isAllowedImpl AD0 d c = False
isAllowedImpl (AD x a) d c = 
  if d == c then x else isAllowedImpl a d (next c)

instance (OrdEnum d) => MazeCell (AllowedDirections n) d where
  isAllowed a d = isAllowedImpl a d first

{-
class MazeCell cell where
  defaultCell :: cell

class (MazeCell cell) => CellChecker cell dir where
  isAllowed :: cell -> dir -> Bool

class OrdEnum e where
  first :: e
  order :: e -> e

instance CellChecker (OrdEnum a) => c a where
  isAllowed 

data Direction = Left | Right | Up | Down

instance OrdEnum Direction
  first = Left
  order | Left = Right
        | Right = Up
        | Up = Down
        | Down = Left

data Zero = Zero
data Succ a = Succ a

data AllowedDirections = AD Bool Bool Bool Bool

{-
data AllowedDirections succ where
  AD0 :: AllowedDirections Zero
  AD :: Bool -> s -> AllowedDirections (AD s)

x :: AllowedDirections (AD (AD (AD (AD AD0))))
x = AD True True True True
-}
-}
