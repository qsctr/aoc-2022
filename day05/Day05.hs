{-# LANGUAGE BlockArguments #-}

import           Data.List
import           Data.List.Split
import qualified Data.Map        as M

singleMove stacks = map head . M.elems . foldl' exec stacks
  where exec stks (n, src, dest) = iterate mv stks !! n
          where mv s = M.adjust (head (s M.! src) :) dest $ M.adjust tail src s

multiMove stacks = map head . M.elems . foldl' exec stacks
  where exec s (n, src, dest) =
          M.adjust (take n (s M.! src) ++) dest $ M.adjust (drop n) src s

main = do
  (iStacks, _:iProced) <- break null . lines <$> readFile "input.txt"
  let (stackNames, stackIndices) = unzip $
        filter ((/= ' ') . fst) $ zip (last iStacks) [0..]
      stacks = M.fromList $ zip stackNames $ map (dropWhile (== ' ')) $
        transpose $ map (\row -> map (row !!) stackIndices) $ init iStacks
      parseStep ["move", n, "from", [src], "to", [dest]] = (read n, src, dest)
      proced = map (parseStep . words) iProced
  putStrLn $ singleMove stacks proced
  putStrLn $ multiMove stacks proced
