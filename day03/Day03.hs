{-# LANGUAGE BlockArguments #-}

import           Data.List
import           Data.List.Split
import           Data.Maybe

bothCompartments = sum . map \items ->
  let x = head $ uncurry intersect $ splitAt (length items `div` 2) items
  in  fromJust (elemIndex x $ ['a'..'z'] ++ ['A'..'Z']) + 1

badges = sum . map prio . chunksOf 3
  where prio [a, b, c] = fromJust (elemIndex x $ ['a'..'z'] ++ ['A'..'Z']) + 1
          where x = head $ a `intersect` b `intersect` c

main = do
  rucksacks <- lines <$> readFile "input.txt"
  print $ bothCompartments rucksacks
  print $ badges rucksacks
