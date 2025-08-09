module Main where
import RoomLoader (getRoom)
import RoomSolver (toleranceApproximation)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
    roomFile <- head <$> getArgs
    room <- getRoom roomFile
    printf "%.3f\n" $ toleranceApproximation room 3
