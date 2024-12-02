-- |
-- Module      : Utils.Tuples
-- Description : Contains methods to operate on tuples.
-- Copyright   : (c) Pieter De Clercq, 2024
-- License     : MIT
--
-- Contains methods to operate on tuples.
module Utils.Tuples (module Utils.Tuples) where

-- | Maps a function over both elements of a tuple.
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)
