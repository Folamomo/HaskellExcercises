module Student where

data Student = Student {firstName::String, lastName::String, age::Int} 
   deriving (Show, Read, Eq)
   
listToProcess = [Student "Alicja" "Akla" 21, Student "Batrek" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "Damian" "Dab"  22, Student "Eustachy" "Elo" 20]

nazwiska :: [Student] -> [String]
nazwiska [] = []
nazwiska ((Student i n _):rs) = (i ++ " " ++ n ) : nazwiska rs

krotki :: [Student] -> [(Int, Student)]
krotki = zip [1..]

toString::Student -> String
toString (Student i n w) = "student: " ++ n ++ " " ++ take 1 n ++ ". wiek: " ++ show w 

raport :: [(Int, Student)] -> String
raport = foldl  (\acc (nr, s) -> acc ++ show nr ++ ". " ++ toString s ++ "\n") "" 
