module Room where

-- data model for sotring the room
data Room = Room {
    side :: Double,
    detectors :: [(Double, Double)]
} deriving Show
