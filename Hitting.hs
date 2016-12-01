module Main (main) where

import Data.Char
import Text.Printf

main :: IO ()
main = do
  hits <- readFile "hitting.txt" >>= return . parseHitting
  let p h = putStrLn $ percentage hits h
  p Backrow
  p Right'
  p Left'
  p ThirtyOne
  p One
  p Two
  p Slide

percentage :: [(Hit, Bool)] -> Hit -> String
percentage hits hit = printf "%-10s : %-2.1f%% (%3d / %3d)" (show hit)  (fromIntegral a / fromIntegral b * 100 :: Double) a b
  where
  all = [ a | (b, a) <- hits, hit == b ]
  b = length all
  a = length $ filter id all

parseHitting :: String -> [(Hit, Bool)]
parseHitting = f . filter (/= '.') . filter (not . isSpace) . uncomment
  where
  uncomment = unlines . map (takeWhile (/= '#')) . lines
  f a = case a of
    [] -> []
    'l' :       '!' : rest -> (Left'     , True ) : f rest
    'r' :       '!' : rest -> (Right'    , True ) : f rest
    '1' :       '!' : rest -> (One       , True ) : f rest
    '2' :       '!' : rest -> (Two       , True ) : f rest
    's' :       '!' : rest -> (Slide     , True ) : f rest
    'b' :       '!' : rest -> (Backrow   , True ) : f rest
    '3' : '1' : '!' : rest -> (ThirtyOne , True ) : f rest
    'l' :             rest -> (Left'     , False) : f rest
    'r' :             rest -> (Right'    , False) : f rest
    '1' :             rest -> (One       , False) : f rest
    '2' :             rest -> (Two       , False) : f rest
    's' :             rest -> (Slide     , False) : f rest
    'b' :             rest -> (Backrow   , False) : f rest
    '3' : '1' :       rest -> (ThirtyOne , False) : f rest
    a -> error $ "Unexpected string: " ++ a


data Hit
  = Left'
  | Right'
  | One
  | Two
  | ThirtyOne
  | Slide
  | Backrow
  deriving (Show, Eq)



