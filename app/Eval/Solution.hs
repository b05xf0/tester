module Eval.Solution where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Text (Text)

import GHC.Generics ( Generic )

import qualified Data.Solution as DS
import qualified Eval.Task as ET

data Solution =
  Solution
    { fname    :: Text
    , score    :: Int
    , maxScore :: Int
    , tasks    :: [ET.Task] } deriving (Show, Generic)

instance FromJSON Solution
instance ToJSON Solution

mkEval :: DS.Solution -> [ET.Task] -> Solution
mkEval sol evals = Solution fname score maxScore evals
  where
    fname = DS.fname sol
    score = mkScore evals
    maxScore = mkMaxScore evals

mkScore :: [ET.Task] -> Int
mkScore ts = sum $ ET.score <$> ts

mkMaxScore :: [ET.Task] -> Int
mkMaxScore ts = sum $ ET.maxScore <$> ts