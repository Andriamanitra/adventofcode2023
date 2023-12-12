import Data.List (transpose)

countGalaxies = length . filter ('#'==)

sumDistances mul galCounts =
  let
    f 0 l r = mul * l * r
    f _ l r = l * r
    left = scanl (+) 0 galCounts
    right = scanr (+) 0 galCounts
  in
    sum $ zipWith3 f galCounts left right

main = do
  input <- lines <$> getContents
  let galaxiesByRow = map countGalaxies input
  let galaxiesByCol = map countGalaxies (transpose input)

  putStrLn "Part 1:"
  let part1 = sumDistances 2
  print $ part1 galaxiesByRow + part1 galaxiesByCol

  putStrLn "Part 2:"
  let part2 = sumDistances 1_000_000
  print $ part2 galaxiesByRow + part2 galaxiesByCol
