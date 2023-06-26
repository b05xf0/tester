module Data.Tasklist where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Database.SQLite.Simple ( field, FromRow(..), ToRow(..), Query, SQLData )
import Database.SQLite.Simple.FromRow ( field, FromRow(..), RowParser )

data Tasklist = Tasklist
  { title       :: Text
  , description :: Text 
  , dateUpdated :: UTCTime } deriving (Show, Read, Generic)

instance FromJSON Tasklist
instance ToJSON Tasklist

instance FromRow Tasklist where
  fromRow :: RowParser Tasklist
  fromRow = Tasklist <$> field <*> field <*> field

instance ToRow Tasklist where
  toRow :: Tasklist -> [SQLData]
  toRow (Tasklist title description _) = toRow (title, description)   

sqlCreateTable :: [Query]
sqlCreateTable =
  [ "DROP TABLE IF EXISTS tasklist"
  , "CREATE TABLE tasklist ("
    <> "title TEXT PRIMARY KEY,"
    <> "description TEXT NOT NULL,"
    <> "date_updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL"
    <> ")"
  , "DROP TABLE IF EXISTS tasklist_task"
  , "CREATE TABLE tasklist_task ("
    <> "task_name TEXT NOT NULL,"
    <> "tasklist_title TEXT NOT NULL,"
    <> "date_updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,"  
    <> "PRIMARY KEY(task_name, tasklist_title),"
    <> "FOREIGN KEY(task_name) REFERENCES task(name),"
    <> "FOREIGN KEY(tasklist_title) REFERENCES tasklist(title)"
    <> ")" ]

sqlInsert :: Query
sqlInsert = "INSERT INTO tasklist (title, description) VALUES (?,?)"

sqlInsertTask :: Query
sqlInsertTask = "INSERT INTO tasklist_task (task_name, tasklist_title) VALUES (?,?)"

sqlDelete :: Query
sqlDelete = "DELETE FROM tasklist WHERE title = ?"

sqlDeleteTasks :: Query
sqlDeleteTasks = "DELETE FROM tasklist_task WHERE tasklist_title = ?"

sqlSelectTasks :: Query
sqlSelectTasks = "SELECT t.* from tasklist_task tl JOIN task t on tl.task_name = t.name where tl.tasklist_title = ? order by t.score"

sqlSelect :: Query
sqlSelect = "SELECT * from tasklist where title = ?"

sqlSelectAll :: Query
sqlSelectAll = "SELECT * from tasklist"

defaultDescription :: Text
defaultDescription = "## Nyilatkozat\n\nA feladatsor beadásával az alábbi nyilatkozatot elfogadja a megoldó:\n\n\"Nyilatkozom, hogy a vizsgán beadott megoldásomat minden tekintetben önállóan készítettem. Nem megengedett eszközt és nem megengedett külső segítséget nem vettem igénybe, illetve másnak sem nyújtottam.\"\n\n## Általános tudnivalók\n\n- A feladatokat **nem** kell sorrendben megoldani! A feladatokhoz tartozó pontszám nehézség szerint különbözik.\n- Érdemes lehet importálni a következő modulokat: `Data.Char`, `Data.List`, `Data.Foldable`.\n- A megoldásnak meg kell felelnie a feladathoz tartozó függvényszignatúráknak.\n- A beküldött megoldásnak **le kell fordulnia**.\n- A következő szabványkönyvtárbeli függvények segíthetnek a megoldás során: `(!!)`, `(++)`, `ceiling`, `cycle`, `div`, `drop`, `elem`, `even`, `filter`, `floor`, `foldl`, `foldr`, `fromIntegral`, `head`, `isDigit`, `isAlpha`, `isLetter`, `isPrefixOf`, `length`, `lines`, `map`, `maximum`, `minimum`, `mod`, `null`, `odd`, `pi`, `repeat`, `replicate`, `reverse`, `show`, `splitAt`, `sqrt`, `tail`, `take` (lásd [Hoogle](https://hoogle.haskell.org)).\n- Ha valamelyik feladatot nem tudjátok megoldani úgy, hogy leforduljon, akkor hagyjátok meg a típus deklarációt és a definíció legyen `undefined`, továbbá **a nem forduló megoldásodat hagyd ott kommentben**, így lehet, hogy kapsz rá részpontot! Pl.:\n\n```haskell\nfuggveny :: Int -> Int\nfuggveny = undefined\n{-\nfuggveny n = \"foo\"-}\n```\n\nFeladat beadási módja:\n\n- A megoldást egy **.hs** kiterjesztésű fájlba írjátok\n\n- Ezt a **.hs** fájlt csomagoljátok be egy .zip állományba\n- Az így kapott **.zip** állományt töltsétek fel Canvasba, a feladat megoldásaként\n\n## Feladatok\n\n"
