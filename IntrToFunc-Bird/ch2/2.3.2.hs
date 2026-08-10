data Direction = North | East | South | West
    deriving (Eq, Ord, Enum, Show)

reverse :: Direction -> Direction
reverse d = toEnum ((fromEnum d - 2) `mod` 4) 

