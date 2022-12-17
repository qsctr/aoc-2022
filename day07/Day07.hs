import qualified Data.Map as M

data Dir = Dir (M.Map String Dir) [(String, Int)]

atMost100000 = fst . sumSizes
  where sumSizes (Dir dirs files) =
          (sum dirSumSizes + if size <= 100000 then size else 0, size)
          where (dirSumSizes, dirSizes) = unzip $ map sumSizes (M.elems dirs)
                size = sum (dirSizes ++ map snd files)

smallestDelete fs = minimum $
    filter (>= (30000000 - (70000000 - head allSizes))) allSizes
  where allSizes = sizes fs
        sizes (Dir dirs files) =
          sum (map head dirSizes ++ map snd files) : concat dirSizes
          where dirSizes = map sizes (M.elems dirs)

main = do
  let build z [] = fst $ root z
      build z@(dir@(Dir dirs _), ctxs) (("$" : cmd) : lns) = case cmd of
        ["cd", d] -> case d of
          "/"  -> build (root z) lns
          ".." -> build (up z) lns
          nm   -> build (dirs M.! nm, (nm, dir) : ctxs) lns
        ["ls"] -> build (foldr fillDir (Dir dirs []) list, ctxs) lns'
          where fillDir ln (Dir drs fls) = case ln of
                  ["dir", nm] ->
                    Dir (M.insertWith (const id) nm (Dir mempty []) drs) fls
                  [sz, nm] -> Dir drs ((nm, read sz) : fls)
                (list, lns') = break ((== "$") . head) lns
      root z = case snd z of
        [] -> z
        _  -> root $ up z
      up (dir, (nm, Dir dirs files) : ctxs) =
        (Dir (M.insert nm dir dirs) files, ctxs)
  fs <- build (Dir mempty [], []) . map words . lines <$> readFile "input.txt"
  print $ atMost100000 fs
  print $ smallestDelete fs
