module Oak.Core.Room.Bot where

import Oak.Core.Booster
import Data.Maybe
import Data.List

type Score = Double

andPred :: [a -> Bool] -> a -> Bool
andPred preds a = and . fmap ($a) $ preds
orPred :: [a -> Bool] -> a -> Bool
orPred preds a = or . fmap ($a) $ preds

isEntry :: Card -> Bool
isEntry = andPred [(==Nothing) . cardReq, (==Friend) . cardType]

countEntry :: [Card] -> [(Color, [Card])]
countEntry cards = fmap (\(c, cs) -> (c, filter isEntry cs)) . countColors $ cards

countColors :: [Card] -> [(Color, [Card])]
countColors cards = fmap (\c -> (c, filter (\card -> c `elem` cardColors card) cards)) colors
  where
    colors = nub . concat . fmap cardColors $ cards

getCount :: Eq a => [a] -> [(a, [Card])] -> Int
getCount [] _ = 0
getCount k cards = fromMaybe 0 (sum <$> traverse (fmap length . flip lookup cards) k)

score :: [Card] -> [Card] -> [(Card, Score)]
score pool [] = []
score pool (card:cards) = (card, totalScore * totalModifier) : score pool cards
  where
    colors = map fst . take 2 . reverse . sortBy (\a b -> compare (length . snd $ a) (length . snd $ b)) $ countColors pool
    sameColors = fromIntegral $ getCount (cardColors card) $ countColors pool
    entries = fromIntegral $ getCount (cardColors card) $ countEntry pool
    power = maybe 0 (if cardType card == Friend then id else const 0) (cardPower card)
    colorScore :: Score
    colorScore = (if (and . map (\c -> or $ map (c==) colors) . cardColors $ card) then (max 5 (20 - sameColors)) else sameColors*0.5) + entries * 0.3
    efficiencyScore :: Score
    efficiencyScore = if power == 0 then 1 else fromMaybe 0 (fromIntegral <$> cardCost card) / fromIntegral power
    entryScore :: Score
    entryScore = max 1 (if isEntry card then 6 - (entries / 2) else 1)
    totalScore :: Score
    totalScore = colorScore + efficiencyScore + entryScore
    totalModifier :: Double
    totalModifier = if cardType card == Problem then 0 else 1

sortedScore :: [Card] -> [Card] -> [(Card, Score)]
sortedScore pool draft = reverse . sortBy (\a b -> compare (snd a) (snd b)) . score pool $ draft
