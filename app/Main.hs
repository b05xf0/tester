module Main where

import Prelude hiding ( readFile, writeFile, putStrLn )

import Data.Text.IO as TIO ( putStrLn, readFile, writeFile ) 
import Data.Text ( Text, pack )
import Data.ByteString.Lazy as B ( putStr )
import Data.Function (on)
import Data.List (sortBy)
import Data.Aeson ( ToJSON )
import Control.Monad (when)

import Commands ( Command(..), cmdLineParser )
import Parser ( parseTasks )
import qualified Eval as E 
import qualified Report as R
import qualified Data as D
import qualified Random as Rnd

main :: IO ()
main = cmdLineParser >>= work

work :: Command -> IO ()
work command = case command of
  EvalSolution
    { solutionFiles       = fs
    , tasklistTitle       = tl
    , concurrently        = c
    , markdown            = m  } -> evalSolutions fs tl c m
  AddTasks
    { tasksFile           = f  } -> addTasks f
  GenTasklist
    { specs               = s
    , tasklistTitle       = t
    , tasklistDescription = d  } -> genTaskList t s d
  QueryTasks                     -> queryTasks
  QueryTasklists                 -> queryTasklists
  QueryTasklistTasks
    { tasklistTitle       = t
    , markdown            = m  } -> queryTasklist t m
  GenSolution
    { tasklistTitle       = t
    , solutionFile        = mf } -> genSolution t mf
  InitDb                         -> initDb

say :: ToJSON a => a -> IO ()
say sth = (B.putStr . R.toJson) sth >> putStrLn " "

initDb :: IO ()
initDb = D.init >> say (R.ToSay (Right "Database has been initialized"))

addTasks :: FilePath -> IO ()
addTasks f = do
  src <- readFile f
  case parseTasks src of
    Left error   -> say (R.ToSay (Left error))
    Right parsed -> D.saveTasks parsed >> say (R.ToSay (Right "Tasks have been added to database"))

evalSolutions :: [FilePath] -> D.ID -> Bool -> Bool -> IO ()
evalSolutions fs tl c m = do
  sources <- mapM readFile fs
  tasks   <- D.selectTasksByTasklist tl
  let sols = zipWith3 D.mkSolution [1..] sources fs
  evals   <- E.runTests c tasks sols
  let report = R.EvalReport tl evals
  (if m then TIO.putStrLn (R.evalToMd report) else say report)

genTaskList :: D.ID -> [D.TasklistSpec] -> Maybe Text -> IO ()
genTaskList tlTitle specs md = do
  tasks          <- D.selectTasks
  randomized     <- Rnd.shuffleList tasks
  let newTasks    = concatMap (getTasks randomized) specs
      newTasklist = D.mkTasklist tlTitle md
  D.saveTasklist newTasks newTasklist
  say (R.ToSay (Right "New tasklist has been generated"))
  where
    getTasks :: [D.Task] -> D.TasklistSpec -> [D.Task]
    getTasks ts (score, n) = take n $ filterByScore ts score

    filterByScore :: [D.Task] -> Int -> [D.Task]
    filterByScore ts score = [t | t <- ts, score == D.getTaskScore t]

queryTasklist :: D.ID -> Bool -> IO ()
queryTasklist title m = do
  tasklist <- D.selectTasklist title
  tasks    <- D.selectTasksByTasklist title
  tsReport <- mapM D.getProps tasks
  let report = (tasklist, R.mkTaskReport <$> tsReport)
  (if m then TIO.putStrLn (R.tasklistToMd report) else say report)

queryTasks :: IO ()
queryTasks = D.selectTasks >>= mapM D.getProps >>= say

queryTasklists :: IO ()
queryTasklists = D.selectTasklists >>= say


genSolution :: D.ID -> FilePath -> IO ()
genSolution title f = do
  tasks       <- D.selectTasksByTasklist title
  writeFile f (R.taskSolutionsToHs tasks)
  say (R.ToSay (Right "Solution source has been generated"))
