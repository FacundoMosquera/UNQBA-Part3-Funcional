data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving (Eq, Ord, Enum, Show)

dayBefore :: Day -> Day 
dayBefore d = toEnum ((fromEnum d - 1) `mod` 7)