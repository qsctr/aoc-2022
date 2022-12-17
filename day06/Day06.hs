import           Data.List
import           Data.Maybe

startOfPacket = (+ 4) . fromJust . findIndex first4Unique . tails
  where first4Unique s = let x = take 4 s in nub x == x

startOfMessage = (+ 14) . fromJust . findIndex first14Unique . tails
  where first14Unique s = let x = take 14 s in nub x == x

main = do
  buf <- readFile "input.txt"
  print $ startOfPacket buf
  print $ startOfMessage buf
