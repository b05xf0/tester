module Data.Task where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )
import Data.Text ( Text )
import Data.Time (UTCTime)
import Database.SQLite.Simple ( field, FromRow(..), ToRow(..), Query, SQLData )
import Database.SQLite.Simple.FromRow ( field, FromRow(..), RowParser )

data Task =
  Task
  { name        :: Text  
  , description :: Text
  , signature   :: Text
  , solution    :: Text
  , score       :: Int 
  , dateUpdated :: UTCTime } deriving (Show, Read, Eq, Generic)

instance FromJSON Task
instance ToJSON Task

instance FromRow Task where
  fromRow :: RowParser Task
  fromRow = Task <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Task where
  toRow :: Task -> [SQLData]
  toRow (Task name description signature solution score _) = toRow (name, description, signature, solution, score)  

sqlCreateTable :: [Query]
sqlCreateTable =
  [ "DROP TABLE IF EXISTS task"
  , "CREATE TABLE task ("
    <> "name         TEXT PRIMARY KEY,"
    <> "description  TEXT NOT NULL,"
    <> "signature    TEXT NOT NULL,"
    <> "solution     TEXT NOT NULL,"
    <> "score        INTEGER NOT NULL,"
    <> "date_updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL"
    <> ")" ]

sqlInsert :: Query
sqlInsert = "INSERT INTO task (name, description, signature, solution, score) VALUES (?,?,?,?,?)"

sqlDelete :: Query
sqlDelete = "DELETE FROM task WHERE name = ?"

sqlDeleteProps :: Query
sqlDeleteProps = "DELETE FROM property WHERE task_name = ?"

sqlSelectTasks :: Query
sqlSelectTasks = "SELECT * FROM task"
