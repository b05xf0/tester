module Data.Property where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Database.SQLite.Simple ( field, FromRow(..), ToRow(..), Query, SQLData )
import Database.SQLite.Simple.FromRow ( field, FromRow(..), RowParser )


data Property =
  Property
    { name          :: Text
    , taskName      :: Text
    , source        :: Text
    , score         :: Int 
    , dateUpdated   :: UTCTime } deriving (Show, Read, Generic)

instance FromJSON Property
instance ToJSON Property

instance FromRow Property where
  fromRow :: RowParser Property
  fromRow = Property <$> field <*> field <*> field <*> field <*> field

instance ToRow Property where
  toRow :: Property -> [SQLData]
  toRow (Property name taskName source score _) = toRow (name, taskName, source, score)

sqlCreateTable :: [Query]
sqlCreateTable =
  [ "DROP TABLE IF EXISTS property"
  , "CREATE TABLE property ("
    <> "name TEXT NOT NULL,"
    <> "task_name TEXT NOT NULL,"
    <> "source TEXT NOT NULL,"
    <> "score INTEGER NOT NULL,"
    <> "date_updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,"
    <> "PRIMARY KEY(name, task_name)"
    <> "FOREIGN KEY(task_name) REFERENCES task(name)"
    <> ")" ]

sqlInsert :: Query
sqlInsert = "INSERT INTO property (name, task_name, source, score) VALUES (?,?,?,?)"

sqlSelect :: Query
sqlSelect = "SELECT * FROM property WHERE task_name = ?"