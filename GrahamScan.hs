{-
Copyright 2011 Alexander Midgley

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module GrahamScan(grahamScan, Point(..), point) where

import Data.Function (on)
import Data.List (sortBy)
import Prelude hiding (Either(..))

data Turn = Left | Right | Colinear
            deriving (Show, Eq)

data Point a = Point {px :: a, py :: a}
               deriving Show

point = uncurry Point

grahamScan :: (Floating a, Ord a) => [Point a] -> [Point a]
grahamScan points
    | length points < 3 = error "Degenerate"
    | otherwise         = 
        let (firstPoint:rest) = findFirstPoint points
            sortedRest = sortBy (compare `on` (angle firstPoint)) rest

            loop (a:b:[]) = case turn a b firstPoint of
                              Left -> b : []
                              _    -> []
            loop (a:b:c:ps) = case turn a b c of
                                Left -> b : loop (b:c:ps)
                                _    -> loop (a:c:ps)

        in firstPoint : loop (firstPoint:sortedRest)

findFirstPoint points
    | null points = error "Null points"
    | otherwise   = loop points [] where
    loop (a:[]) ps = a:ps
    loop (a:b:rest) ps =
        if (py a, px a) < (py b, px b)
        then loop (a:rest) (b:ps)
        else loop (b:rest) (a:ps)

angle a b = (dx / len, len) where
    dx = px a - px b
    dy = py a - py b
    len = sqrt (dx * dx + dy * dy)

turn a b c = case compare cross 0 of
               GT -> Left
               EQ -> Colinear
               LT -> Right
    where
      cross = x1 * y2 - x2 * y1
      x1 = px b - px a
      y1 = py b - py a
      x2 = px c - px b
      y2 = py c - py b
