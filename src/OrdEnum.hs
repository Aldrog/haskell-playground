module OrdEnum where

class (Eq e) => OrdEnum e where
  first :: e
  next :: e -> e
