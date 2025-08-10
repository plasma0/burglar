module RoomSolver(toleranceApproximation) where

import Room

import Data.Matrix (Matrix, matrix, (!), safeGet, setElem)
import Data.Functor(($>))
import Data.Maybe (isNothing)
import Data.Set(Set)
import qualified Data.Set as Set

data Cell = Wall | Empty deriving Eq

-- how much "resolution" should discrete maze have
-- to this maze continuous field will be mapped
-- 50 seems to be good enough for three digit precision
-- and on average PC yeilds almost instant result
granuality :: Int
granuality = 50

-- initial position of the burglar and the valut
-- assumed, as required, to be on the midpoints of the opposing walls
startingPosition :: (Int, Int)
startingPosition = (granuality `div` 2, 1)

valutPosition :: (Int, Int)
valutPosition = (granuality `div` 2, granuality)


-- function calculating possibility of being detected at given position
-- by given detector
detection :: Double -> (Double, Double) -> (Double, Double) -> Double
detection roomL (detX, detY) (posX, posY) = let distance = sqrt ((detX-posX)^2 + (detY-posY)^2)
                                             in exp (-((pi*distance/roomL)^2))

-- function calculating possibility of being detected at given position
-- taking into account all present detectors
-- it works by taking possiblility of being detected by most sensitive detector at given location
detections :: Room -> (Double, Double) -> Double
detections (Room _ []) _ = 0.0
detections (Room side detectors) point = maximum $ detection side point <$> detectors

-- this one generates "heat map" for given room and supplied tolerance parmeter
-- output is effectively maze in which walls are areas in which you cannot enter
-- if you don't want to raise your possibility of being detected beyond tolerance parameter 
toDiscreteMaze :: Double -> Room -> Matrix Cell
toDiscreteMaze tolerance room@(Room side _) = matrix granuality granuality discreteMazeGenerator
                                              where discreteMazeGenerator (x,y) = if detections room (pixel * fromIntegral x, pixel * fromIntegral y) < tolerance
                                                                                        then Empty
                                                                                        else Wall
                                                    pixel = side / fromIntegral granuality

-- this is simple maze solving algorithm based on breadth first search method
-- NOTE
-- this can be used only to determine if maze has a solution since for performance
-- it does not store paths
solve :: Matrix Cell -> (Int, Int) -> (Int, Int) -> Bool
solve maze source destination = let beenThere = maze $> False
                                 in layer (Set.singleton source) beenThere
                              where layer candidates visitedCells | destination `elem` candidates = True
                                                                  | otherwise = let anotherVisitedCells = updateVisited visitedCells candidates
                                                                                 in let newCells = Set.filter (checkCell' anotherVisitedCells) (probeCells candidates)
                                                                                     in if null newCells
                                                                                            then False
                                                                                            else layer newCells anotherVisitedCells
                                    checkCell' = checkCell maze

-- following functions are just helpers for maze solver

-- this one prevents from
-- searching beyond the maze's plane
-- crossing walls
-- doubling paths
checkCell :: Matrix Cell -> Matrix Bool -> (Int, Int) -> Bool
checkCell maze beenThere (x,y) | isNothing (safeGet x y maze) = False
                               | maze ! (x,y) == Wall = False
                               | beenThere ! (x,y) = False
                               | otherwise = True

-- this one is simply for generating targets for further search
probeCells :: Set (Int, Int) -> Set (Int, Int)
probeCells = foldr (\a b -> fourNeighbours a <> b) mempty
                 where fourNeighbours (x,y) = Set.fromList [(x+1,y)
                                                           ,(x-1,y)
                                                           ,(x,y-1)
                                                           ,(x,y+1)]

-- this function add visited cells to data storage
-- to prevent exploring same area
updateVisited :: Matrix Bool -> Set (Int, Int) -> Matrix Bool
updateVisited = foldl (flip (setElem True))

-- this function determines minimal possibility of being detected
-- by increasing tolerance parameter and trying to find a path in which it won't be exceeced
-- it approximates the possibility to given decimal place in a similar manner to binary search
-- and computing square root
-- in the first iteration it tries tolearce parameters that differs by 0.1 and when it finds
-- one that not matches followed by one that does it go down to the next iteration
-- starting with the first one this time increasing tolerance by 0.01 and so one
toleranceApproximation :: Room -> Int -> Double
toleranceApproximation room decimalPlace = go 1 0.0
                                     where go iter position | iter > decimalPlace = position
                                                            | solve (toDiscreteMaze position room) startingPosition valutPosition /= solve (toDiscreteMaze (position+(1/(10^iter))) room) startingPosition valutPosition = go (iter+1) position
                                                            | otherwise = go iter (position+(1/(10^iter)))
