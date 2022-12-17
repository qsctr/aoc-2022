{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

visible trees = V.sum $ V.imap (\i r -> U.sum $ U.imap (isVisible r i) r) trees
  where isVisible row i j x = fromEnum $
          V.all (< x) u || V.all (< x) d || U.all (< x) l || U.all (< x) r
          where (u, V.tail -> d) = V.splitAt i $ V.map (U.! j) trees
                (l, U.tail -> r) = U.splitAt j row

scenic trees = V.maximum $ V.imap (\i r -> U.maximum $ U.imap (score i) r) trees
  where score i j x = look (-1) 0 * look 1 0 * look 0 (-1) * look 0 1
          where look di dj = go i j
                  where go i' j' = case trees V.!? i'' >>= (U.!? j'') of
                          Just y
                            | y < x -> succ $ go i'' j''
                            | otherwise -> 1 :: Int
                          Nothing -> 0
                          where i'' = i' + di
                                j'' = j' + dj

main = do
  trees <- V.fromList . map (U.fromList . map (read @Int . pure)) . lines
    <$> readFile "input.txt"
  print $ visible trees
  print $ scenic trees
