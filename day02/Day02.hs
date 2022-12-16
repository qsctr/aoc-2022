{-# LANGUAGE BlockArguments #-}

import           Data.List

asMove = sum . map \(ol, ml) ->
  let Just o = elemIndex ol ['A'..]
      Just m = elemIndex ml ['X'..]
  in  m + 1 + (m - o + 1) `mod` 3 * 3

asEnd = sum . map \(ol, el) ->
  let Just o = elemIndex ol ['A'..]
      Just e = elemIndex el ['X'..]
  in  (o + e - 1) `mod` 3 + 1 + e * 3

main = do
  guide <- map (\[o, ' ', m] -> (o, m)) . lines <$> readFile "input.txt"
  print $ asMove guide
  print $ asEnd guide
