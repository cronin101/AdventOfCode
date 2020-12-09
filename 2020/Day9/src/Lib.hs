module Lib
  ( loadInput
  , initialiseStream
  , checkHead
  , playStream
  , findSegmentWithValue
  ) where

import qualified Data.ByteString.Char8         as BSC
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                , mapMaybe
                                                )
import           Data.Dequeue                   ( BankersDequeue )
import qualified Data.Dequeue                  as D
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IM
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.List                      ( tails )
import           Data.Bifunctor                 ( Bifunctor(second) )

data XMASStream = XMASStream
  { preamble       :: BankersDequeue Int
  , preambleCounts :: IntMap Int
  , sumsToPairs    :: IntMap (Set (Int, Int))
  , remaining      :: [Int]
  }
  deriving Show

preamblePairs :: [Int] -> [(Int, Int)]
preamblePairs xs =
  [ normalizePair (x, y) | (x : ys) <- tails xs, y <- ys, x /= y ]

normalizePair :: (Int, Int) -> (Int, Int)
normalizePair (x, y) | x < y     = (x, y)
                     | otherwise = (y, x)

initialiseStream :: Int -> [Int] -> XMASStream
initialiseStream preambleLength xs = XMASStream dequeue
                                                counts
                                                sumsToPairs
                                                remaining
 where
  (preamble, remaining) = splitAt preambleLength xs
  dequeue               = D.fromList preamble
  counts                = IM.fromListWith (+) $ zip preamble (repeat 1)
  pairs                 = preamblePairs preamble
  sumsToPairs           = IM.fromListWith S.union
    $ map (\pair@(x, y) -> (x + y, S.singleton pair)) pairs

updateCounts :: IntMap Int -> Int -> Int -> IntMap Int
updateCounts counts evicted incoming = if evicted == incoming
  then counts
  else IM.insertWith (+) incoming 1 countsWithoutEvicted
 where
  countsWithoutEvicted = IM.update
    (\count -> if count == 1 then Nothing else Just $ count - 1)
    evicted
    counts

pruneSums
  :: IntMap (Set (Int, Int)) -> Int -> Int -> [Int] -> IntMap (Set (Int, Int))
pruneSums sums evicted incoming = if evicted == incoming
  then const sums
  else foldl
    (\m v -> IM.update (prunePair $ normalizePair (v, evicted)) (v + evicted) m)
    sums
 where
  prunePair pair set =
    let withoutPair = S.delete pair set
    in  if S.null withoutPair then Nothing else Just withoutPair

expandSums
  :: IntMap (Set (Int, Int)) -> Int -> Int -> [Int] -> IntMap (Set (Int, Int))
expandSums sums evicted incoming = if evicted == incoming
  then const sums
  else foldl
    (\m v -> IM.insertWith S.union
                           (v + incoming)
                           (S.singleton $ normalizePair (v, incoming))
                           m
    )
    sums

step :: XMASStream -> XMASStream
step finishedStream@(XMASStream _ _ _ []) = finishedStream
step (XMASStream preamble counts sumsToPairs (value : remaining)) = XMASStream
  preamble'
  counts'
  sumsToPairs'
  remaining
 where
  Just (evicted, preamble') =
    second (`D.pushBack` value) <$> D.popFront preamble
  counts'               = updateCounts counts evicted value
  shouldPrune           = isNothing $ IM.lookup evicted counts'
  shouldExpand          = isNothing $ IM.lookup value counts
  otherValuesInPreamble = filter (/= evicted) $ map fst $ IM.assocs counts
  prunedSumsToPairs     = if shouldPrune
    then pruneSums sumsToPairs evicted value otherValuesInPreamble
    else sumsToPairs
  sumsToPairs' = if shouldExpand
    then expandSums prunedSumsToPairs evicted value otherValuesInPreamble
    else prunedSumsToPairs

playStream :: XMASStream -> [XMASStream]
playStream = takeWhile ((not . null) . remaining) . iterate step

checkHead :: XMASStream -> Maybe (Int, Bool)
checkHead (XMASStream _ _ _ []) = Nothing
checkHead (XMASStream _ _ sumsToPairs (value : _)) =
  case IM.lookup value sumsToPairs of
    Nothing -> Just (value, False)
    Just _  -> Just (value, True)

findSegmentWithValue :: [Int] -> Int -> [Int]
findSegmentWithValue xs v = take (endIndex - startIndex) $ drop startIndex xs
 where
  Just startIndex      = IM.lookup startValue cumSumToIndex
  startValue           = endValue - v
  (endValue, endIndex) = IM.findMin $ IM.filterWithKey
    (\k _ -> k /= v && isJust (IM.lookup (k - v) cumSumToIndex))
    cumSumToIndex
  cumSumToIndex = IM.fromDistinctAscList $ zip (scanl1 (+) xs) [1 ..]

loadInput :: String -> IO [Int]
loadInput fileName =
  mapMaybe (fmap fst . BSC.readInt) . BSC.lines <$> BSC.readFile
    ("src/" ++ fileName)

