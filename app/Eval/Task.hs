module Eval.Task where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Text (Text)

import GHC.Generics ( Generic )

import qualified Data.Task as DT
import qualified Eval.Property as EP

data Task =
  Task
    { name     :: Text
    , score    :: Int
    , maxScore :: Int
    , props    :: [EP.Property] } deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

mkEval :: DT.Task -> [EP.Property] -> Task
mkEval task evals = Task name score maxScore evals
  where
    name = DT.name task
    score = mkScore task evals
    maxScore = DT.score task
   
mkScore :: DT.Task -> [EP.Property] -> Int
mkScore t ps = if all EP.passed ps then DT.score t else maximum $ EP.score <$> ps