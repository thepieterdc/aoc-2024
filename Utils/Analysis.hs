-- |
-- Module      : Utils.Analysis
-- Description : Contains reusable methods for mathematical analysis.
-- Copyright   : (c) Pieter De Clercq, 2024
-- License     : MIT
--
-- Contains reusable methods for mathematical analysis.
module Utils.Analysis (module Utils.Analysis) where

import Utils.Grid (Coordinate)

-- | Finds the slope between two coordinates.
slope :: Coordinate -> Coordinate -> Double
slope (x1, y1) (x2, y2) = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)
