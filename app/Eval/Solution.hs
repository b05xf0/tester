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


{-
{-# LANGUAGE OverloadedStrings #-}

module Eval.Solution where

import Prelude hiding ( unlines )

import Data.Text ( Text, pack, unlines )
import qualified Eval.Task as ET
import qualified Data.Solution as DS
import qualified Data.Tasklist as DTL

data Solution = Solution
  {
    details :: DS.Solution,
    evals :: [ET.Task]
  } deriving (Show)

score :: Solution -> Int
score s = sum . map ET.score $ evals s

maxScore :: Solution -> Int
maxScore s = sum . map ET.maxScore $ evals s

result :: Solution -> Text
result s = (pack . show $ score s) <> "/" <> (pack . show $ maxScore s) <> " points"
-}