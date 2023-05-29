module Random where

import System.Random ( newStdGen, Random(randoms) )
import Data.List (sortBy)
import Data.Function (on)

getRandoms :: IO [Float]
getRandoms = randoms <$> newStdGen

shuffleList :: [b] -> IO [b]
shuffleList xs = do
  rs <- getRandoms
  return $ fst <$> sortBy (compare `on` snd) (zip xs rs)
  