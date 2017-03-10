{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.UUID.SafeCopy where
import Data.UUID
import Data.SafeCopy

instance SafeCopy UUID where
  putCopy = contain . safePut . toWords
  getCopy = contain $ uncurry4 fromWords <$> safeGet
    where uncurry4 f (a, b, c, d) = f a b c d

