import           Data.List
import           Data.List.Split
import           Data.Ord

mostCalories = maximum . map sum

top3Calories = sum . take 3 . sortOn Down . map sum

main = do
  calories <- map (map read) . splitWhen null . lines <$> readFile "input.txt"
  print $ mostCalories calories
  print $ top3Calories calories
