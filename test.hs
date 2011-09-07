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

module Main where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Time.Clock
import System.IO

import GrahamScan

instance NFData a => NFData (Point a) where
    rnf (Point xx yy) = rnf xx `seq` rnf yy `seq` ()

makePoint [x, y] =
    let sx = read x
        sy = read y
    in sx `seq` sy `seq` Point sx sy

evaluate points = do
  start <- getCurrentTime
  grahamScan points `deepseq` return ()
  end <- getCurrentTime
  hPutStrLn stderr . show $ diffUTCTime end start

main = do
  coords <- fmap (map words . lines) getContents
  let points :: [Point Float]
      points = parMap rdeepseq makePoint coords
  points `seq` evaluate points