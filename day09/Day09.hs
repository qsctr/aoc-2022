import           Data.List
import qualified Data.Set  as S

twoKnots = numVisited . foldl' move ((0, 0), (0, 0), S.singleton (0, 0))
  where move s (d, n) = iterate mv s !! n
          where mv (h@(hx, hy), t@(tx, ty), v) = (h', t', S.insert t' v)
                  where h'@(hx', hy') = case d of
                          'U' -> (hx, hy + 1)
                          'D' -> (hx, hy - 1)
                          'L' -> (hx - 1, hy)
                          'R' -> (hx + 1, hy)
                        t'
                          | abs (hx' - tx) <= 1, abs (hy' - ty) <= 1 = t
                          | otherwise = h
        numVisited (_, _, v) = S.size v

tenKnots = S.size . snd . foldl' move (replicate 10 (0, 0), S.singleton (0, 0))
  where move s (d, n) = iterate mv s !! n
          where mv (h@(hx, hy):ks, v) = (ks', S.insert (last ks') v)
                  where ks' = scanl' follow h' ks
                        h' = case d of
                          'U' -> (hx, hy + 1)
                          'D' -> (hx, hy - 1)
                          'L' -> (hx - 1, hy)
                          'R' -> (hx + 1, hy)
        follow (hx', hy') (tx, ty)
          | abs dx <= 1, abs dy <= 1 = (tx, ty)
          | otherwise = (tx + signum dx, ty + signum dy)
          where dx = hx' - tx
                dy = hy' - ty

main = do
  motions <- map (\(d:' ':n) -> (d, read n)) . lines <$> readFile "input.txt"
  print $ twoKnots motions
  print $ tenKnots motions
