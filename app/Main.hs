module Main where
import ISL (serialize, testISL)

main :: IO ()
main = putStrLn $ serialize testISL
