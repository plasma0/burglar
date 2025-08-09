module RoomLoader where

import Room

import Text.Trifecta hiding (double)
import Control.Monad (replicateM)


-- parsers for numbers
int :: Parser Int
int = read <$> some digit

double :: Parser Double
double = do
    totalPart <- some digit
    sep <- char '.'
    factorialPart <- some digit
    return $ read (totalPart ++ sep : factorialPart)

-- parser for single pair of coordinates
pair :: Parser (Double, Double)
pair = do
    x <- double
    someSpace
    y <- double
    return (x,y)

-- parser for whole file describing room that leads to the valut
-- CAUTION
-- as required in the task the parser will stop consuming
-- detectors location after reaching number specified in the second line of file
room :: Parser Room
room = do
    side <- double <* newline
    numberOfDetectors <- int <* newline
    detectors <- replicateM numberOfDetectors (pair <* newline)
    return (Room side detectors)

--- high level function to get the room from file
getRoom :: FilePath -> IO Room
getRoom filePath = do
    maybeRoom <- parseFromFile room filePath
    case maybeRoom of
        Just parsedRoom -> return parsedRoom
        Nothing -> error "parsing error"