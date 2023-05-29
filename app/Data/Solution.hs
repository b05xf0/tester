module Data.Solution where

import Data.Text ( Text )

data Solution = 
  Solution 
    { idx    :: Int
    , source :: Text 
    , fname  :: Text } deriving (Show, Read)

