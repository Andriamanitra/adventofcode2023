import Data.List (sort, sortBy, sortOn, group, elemIndex)
import Data.Maybe (fromJust)
import Data.Function (on)

data Hand = Hand
  { cards :: [Char]
  , bid :: Int
  } deriving (Show, Eq)

instance Ord Hand where
  compare = compare `on` (score.cards)
    where
      score cs = (counts cs, map cardValue cs)
      cardValue c = fromJust $ elemIndex c "23456789TJQKA"

newtype WithJokers = WithJokers
  { hand :: Hand
  } deriving (Show, Eq)

instance Ord WithJokers where
  compare = compare `on` (score.cards.hand)
    where
      score cs = (countsWithJoker 'J' cs, map cardValue cs)
      cardValue c = fromJust $ elemIndex c "J23456789TQKA"

countsWithJoker :: Ord a => Eq a => a -> [a] -> [Int]
countsWithJoker jokerChar cards
  | (x:xs) <- nonJokerCounts = x + nJokers : xs
  | otherwise = [nJokers]
  where
    nonJokerCounts = counts $ filter (/= jokerChar) cards
    nJokers = length $ filter (==jokerChar) cards

counts :: Ord a => [a] -> [Int]
counts = sortBy (flip compare) . map length . group . sort

readHand :: String -> Hand
readHand line = Hand cards (read bid)
  where [cards, bid] = words line

winnings :: [Hand] -> Int
winnings = sum . zipWith (*) [1..] . map bid

main :: IO ()
main = do
  hands <- map readHand . lines <$> getContents
  putStrLn "Part 1:"
  print $ winnings $ sort hands
  putStrLn "Part 2:"
  print $ winnings $ sortOn WithJokers hands
