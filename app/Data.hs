{-
Sources:

https://www.sqlite.org/index.html
https://hackage.haskell.org/package/sqlite-simple
https://hackage.haskell.org/package/sqlite-simple-0.4.18.2/docs/Database-SQLite-Simple.html
-}
{-# LANGUAGE TupleSections #-}

module Data
  ( module D, TasklistSpec, ID
  , init, selectTasksByTasklist, selectTasklists, selectTasklist, selectTasks, saveTasks, saveTasklist 
  , getProps, getPropName, getPropSource, getSolSource, getSolIdx, getTaskScore, getTaskName
  , mkTasklist, mkTask, mkProperty, mkSolution ) where

import Prelude hiding (init)

import Paths_tester ( getDataFileName )
import Data.Text ( Text, takeWhileEnd, pack )
import Data.Maybe ( fromMaybe )
import Data.Foldable ( forM_ )
import Database.SQLite.Simple
    ( Only(Only), Connection
    , execute, execute_, query, query_, withConnection )

import qualified Data.Task as DT
import qualified Data.Tasklist as DTL
import qualified Data.Property as DP
import qualified Data.Solution as DS

import Data.Task as D ( Task, Task(Task) )
import Data.Tasklist as D ( Tasklist, Tasklist(Tasklist) )
import Data.Property as  D ( Property, Property(Property) )
import Data.Solution as D ( Solution, Solution(Solution) )
import Data.Time (UTCTime)

type ID = Text
type TasklistSpec = (Int, Int)

withConn :: (Connection -> IO a) -> IO a
withConn action = do
    filePath <- getDataFileName "tester.sqlite"
    withConnection filePath action

init :: IO ()
init = withConn $ \conn -> mapM_ (execute_ conn) scripts
  where
    scripts = DT.sqlCreateTable
           <> DTL.sqlCreateTable
           <> DP.sqlCreateTable

insertProp :: D.Property -> IO ()
insertProp p = withConn $ \conn -> execute conn DP.sqlInsert p

insertTask :: D.Task -> IO ()
insertTask t = withConn $ \conn -> execute conn DT.sqlInsert t

deleteTask :: ID -> IO ()
deleteTask tName = withConn $ \conn -> do
  execute conn DT.sqlDeleteProps (Only tName)
  execute conn DT.sqlDelete (Only tName)

insertTasklist :: D.Tasklist -> IO ()
insertTasklist tl = withConn $ \conn -> execute conn DTL.sqlInsert tl

deleteTasklist :: ID -> IO ()
deleteTasklist tlTitle = withConn $ \conn -> do
  execute conn DTL.sqlDeleteTasks (Only tlTitle)
  execute conn DTL.sqlDelete (Only tlTitle)

insertTasklistTask :: (ID, ID) -> IO ()
insertTasklistTask tlt = withConn $ \conn -> execute conn DTL.sqlInsertTask tlt

selectPropsByTask :: ID -> IO [DP.Property]
selectPropsByTask tId = withConn $ \conn -> query conn DP.sqlSelect (Only tId)

selectTasksByTasklist :: ID -> IO [DT.Task]
selectTasksByTasklist tlId = withConn $ \conn -> query conn DTL.sqlSelectTasks (Only tlId)

selectTasks :: IO [DT.Task]
selectTasks = withConn $ \conn -> query_ conn DT.sqlSelectTasks

selectTasklists :: IO [DTL.Tasklist]
selectTasklists = withConn $ \conn -> query_ conn DTL.sqlSelectAll

selectTasklist :: ID -> IO (Maybe DTL.Tasklist)
selectTasklist tlId = withConn $ \conn -> do
  resp <- query conn DTL.sqlSelect (Only tlId)
  return $ firstOrNothing resp

getTasks :: Tasklist -> IO [DT.Task]
getTasks tl = selectTasksByTasklist $ DTL.title tl

getProps :: Task -> IO (D.Task, [D.Property])
getProps t = do
  ps <- selectPropsByTask $ DT.name t
  return (t, ps)

mkProperty :: (ID, ID, Text,Int) -> Property
mkProperty (name, taskName, source, score)
  = DP.Property name taskName source score defaultTime

getPropName :: Property -> Text
getPropName = DP.name

getPropSource :: Property -> Text
getPropSource = DP.source

mkTask :: ID -> Text -> Text -> Text -> Int -> Task
mkTask name description signature solution score
  = DT.Task name description signature solution score defaultTime

getTaskName :: Task -> Text
getTaskName = DT.name

getTaskScore :: Task -> Int
getTaskScore = DT.score

mkTasklist :: ID -> Maybe Text -> Tasklist
mkTasklist title mDesc 
  = DTL.Tasklist title (fromMaybe DTL.defaultDescription mDesc) defaultTime

mkSolution :: Int -> Text -> String -> Solution
mkSolution idx src fname = Solution idx src $ takeWhileEnd (/='/') $ pack fname

getSolSource :: Solution -> Text
getSolSource = DS.source

getSolIdx :: Solution -> Int
getSolIdx = DS.idx

saveTasks :: (Maybe Tasklist, [(Task,[Property])]) -> IO ()
saveTasks (mtl, ts) = do
  forM_ mtl $ saveTasklist $ fst <$> ts
  forM_ ts saveTask
 
saveTask :: (Task,[Property]) -> IO ()
saveTask (t, ps) = do
  deleteTask (DT.name t)
  insertTask t
  forM_ ps insertProp

saveProperty :: Property -> IO ()
saveProperty = insertProp

saveTasklist :: [Task] -> Tasklist -> IO ()
saveTasklist ts tl = do
  deleteTasklist (DTL.title tl)
  insertTasklist tl
  forM_ tlts insertTasklistTask
  where
    tlts =  (, DTL.title tl) . DT.name <$> ts

firstOrNothing :: [a] -> Maybe a
firstOrNothing xs = case xs of
  []    -> Nothing
  (x:_) -> Just x

defaultTime:: UTCTime
defaultTime = read "1900-01-01 00:00:00 UTC"