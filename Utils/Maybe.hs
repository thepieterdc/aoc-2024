-- |
-- Module      : Utils.Maybe
-- Description : Contains reusable methods to handle Maybe monads.
-- Copyright   : (c) Pieter De Clercq, 2022
-- License     : MIT
--
-- Contains reusable methods to handle Maybe monads.
module Utils.Maybe (module Utils.Maybe) where

-- | Executes the mapping function on the given Maybe value.
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just a) = Just $ f a
mapMaybe _ Nothing = Nothing
