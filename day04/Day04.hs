{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Bifunctor

fullyContains = length . filter \((a, b), (c, d)) ->
  a <= c && d <= b || c <= a && b <= d

overlap = length . filter \((a, b), (c, d)) -> not (b < c || d < a)

main = do
  let splitApp f c = bimap f (f . tail) . break (== c)
  assignments <- map (splitApp (splitApp (read @Int) '-') ',') . lines
    <$> readFile "input.txt"
  print $ fullyContains assignments
  print $ overlap assignments
