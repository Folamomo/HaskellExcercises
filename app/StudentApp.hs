module StdentApp where

import Data.HashMap.Strict

data Student = Student {name::String age::Int id::String}
	deriving (Show, Eq, Read)

type Students = HashMap String Student

menu::IO()
menu = do
    putStrLn "1. Dodaj Studenta"
    putStrLn "2. Wyswietl wszystkich"
    putStrLn "3. Usun Studenta"
    putStrLn "4. Zakoncz"
    getChar

addStudent ::  Students -> IO(Students) 
addStudent students  = do
    str<- getLine
    let student = read str :: Student
    return(insert students (id student) student) 


main::IO()
main = do	
    let students = empty :: Students
    c <- menu
    if c == 
          
