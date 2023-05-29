module Parser ( parseTasks ) where

import Prelude hiding ( lines, unlines, words, unwords )

import Data.Text as T 
  ( Text
  , pack, unpack
  , words, unwords, lines, unlines
  , takeWhile, dropWhile, takeWhileEnd, dropWhileEnd, dropAround
  , isPrefixOf, breakOn, strip )
import Data.List as L ( takeWhile, dropWhile )
import Text.Read ( readMaybe ) 

import Data as D
  ( Tasklist, Task, Property, Solution
  , mkTasklist, mkTask, mkProperty, mkSolution )

type ErrMsg = Text

parseTasks :: Text -> Either ErrMsg (Maybe Tasklist, [(Task, [Property])])
parseTasks src = case (mTl, eTs) of
    (_, Right tasks) -> Right (mTl, tasks)
    (_, Left e)      -> Left e 
  where
    mTl :: Maybe Tasklist
    mTl = case tlInput of
      "" -> Nothing
      _  -> parseTasklist tlInput

    eTs :: Either ErrMsg [(Task, [Property])]
    eTs = case tsInput of
      "" -> Left "Invalid input: missing tasks"
      _  -> mapM parseTask (breaksOn  "###" tsInput)

    (tlInput, tsInput) = breakOn "###" src

parseTasklist :: Text -> Maybe Tasklist
parseTasklist src = case words <$> lines src of
  (("#" : titleWs) : descrLs) -> Just (mkTasklist (unwords titleWs) (Just $ unlines $ unwords <$> descrLs))
  _                           -> Nothing

parseTask :: Text -> Either ErrMsg (Task, [Property])
parseTask src = case (name, description, signature, solution, mScore, eProps) of
  ("", _ , _ , _ , _      , _       ) -> Left $ "Missing task name: "        <> heading
  (_ , "", _ , _ , _      , _       ) -> Left $ "Missing task details: "     <> unlines rest
  (_ , _ , "", _ , _      , _       ) -> Left $ "Missing task signature: "   <> snd breakRest
  (_ , _ , _ , "", _      , _       ) -> Left $ "Missing task solution: "    <> snd breakRest
  (_ , _ , _ , _ , Nothing, _       ) -> Left $ "Invalid task score: "       <> heading
  (_ , _ , _ , _ , _      , Left e  ) -> Left $ "Invalid property: "         <> e
  (_ , _ , _ , _ , Just s , Right ps) -> Right (mkTask name description signature solution s, ps)
  where
    mScore :: Maybe Int
    mScore = (readMaybe . unpack . T.takeWhile (/=')') . T.takeWhileEnd (/='(')) heading

    eProps :: Either ErrMsg [Property]
    eProps = mapM (parseProp name) (breaksOn  "-- prop_" $ snd breakRest'')

    (heading : rest) = strip <$> lines src
    name = (dropAround (=='`') . T.dropWhileEnd (/='`') . T.dropWhile (/='`')) heading
    [description, signature, solution] = strip . fst <$> [breakRest, breakRest', breakRest'']
    breakRest   = breakOn "```haskell"  (unlines rest)
    breakRest'  = breakOn "-- SOLUTION" (unlines . drop 1 . lines . snd $ breakRest)
    breakRest'' = breakOn "-- prop_" (unlines . drop 1 . lines . snd $ breakRest')

parseProp :: Text -> Text -> Either ErrMsg Property
parseProp taskName src = case (name, taskName, source, mScore) of
  ("", _, _ , _           ) -> Left "missing property name"
  (_ , _, "", _           ) -> Left "missing property source"
  (_ , _, _ , Nothing     ) -> Left "invalid property score"
  (_ , _, _ , Just score  ) -> Right $ mkProperty (name, taskName, source, score)
  where
    mScore :: Maybe Int
    mScore = (readMaybe . unpack . T.takeWhile (/=' ') . T.takeWhileEnd (/='(')) heading

    name = strip . T.takeWhile (/=' ') . unwords . drop 1 . words $ heading
    (heading : sourceLs) = lines src
    source = strip . T.dropWhileEnd (=='`') . strip . unlines $ sourceLs
    
breaksOn :: Text -> Text -> [Text]
breaksOn header  = go . lines
  where
    go :: [Text] -> [Text]
    go [] = []
    go (x:xs)
      | isHeader x = unlines (x : L.takeWhile (not . isHeader) xs) : go (L.dropWhile (not . isHeader) xs)
      | otherwise = go xs
    
    isHeader :: Text -> Bool
    isHeader =  isPrefixOf header