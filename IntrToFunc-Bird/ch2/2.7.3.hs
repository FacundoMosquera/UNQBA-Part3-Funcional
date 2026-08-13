
type Day = Int 
type Month = Int 
type Year = Int 
type Date = (Day, Month, Year)

data EMonth = January | February | March | April | May | June | July | August 
            | September | October | November | December 
            deriving (Show, Enum)

toEnumMonth :: Int -> EMonth
toEnumMonth n = toEnum n

showDate :: Date -> String
showDate (d, m, y) = show d ++ " " ++ show (toEnumMonth (m - 1)) ++ " " ++ show y

putDate :: Date -> IO()
putDate = putStr . showDate 