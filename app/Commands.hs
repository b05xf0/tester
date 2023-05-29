{-
https://hackage.haskell.org/package/optparse-applicative
https://hackage.haskell.org/package/optparse-applicative-0.17.0.0/docs/Options-Applicative.html
https://github.com/pcapriotti/optparse-applicative
-}
module Commands (Command (..), cmdLineParser) where

import Options.Applicative
import Data.Text 

data Command
  = EvalSolution 
      { tasklistTitle       :: Text 
      , solutionFiles       :: [FilePath]
      , concurrently        :: Bool
      , markdown            :: Bool }
  | AddTasks
      { tasksFile           :: FilePath }
  | GenSolution
      { tasklistTitle       :: Text
      , solutionFile        :: FilePath }  
  | GenTasklist
      { tasklistTitle       :: Text
      , specs               :: [(Int, Int)]
      , tasklistDescription :: Maybe Text }
  | QueryTasklists
  | QueryTasks
  | QueryTasklistTasks
      { tasklistTitle :: Text
      , markdown      :: Bool }
 | InitDb       

commandParser :: Parser Command
commandParser = subparser
   ( command "eval"
      (info mkEvalOptions           (progDesc "Evaluate solution(s)"))
  <> command "add-tasks"
      (info mkAddTasksOptions       (progDesc "Add (or update) task(s) to database"))
  <> command "gen-solution"
      (info mkGenSolutionOptions    (progDesc "Compile sample solution"))
  <> command "gen-tasklist"
      (info mkGenTasklistOptions    (progDesc "Generate tasklist"))
  <> command "query-tasklists"
      (info mkQueryTasklistsOptions (progDesc "Query tasklists"))
  <> command "query-tasks"
      (info mkQueryTasksOptions     (progDesc "Query tasks"))
  <> command "query-tl-tasks"
      (info mkQueryTasklistOptions  (progDesc "Query tasks by tasklist")) 
  <> command "init-db"
      (info mkInitDbOptions         (progDesc "Initialize database")))

mkAddTasksOptions :: Parser Command
mkAddTasksOptions = AddTasks
  <$> strArgument
      (metavar "FILE" 
      <> help "File name containing tasks (*.md)")

mkEvalOptions :: Parser Command
mkEvalOptions = EvalSolution 
  <$> strArgument
      (metavar "TASKLIST"
      <> help "Title of tasklist") 
  <*> some (argument str 
      (metavar "FILES..."
      <> help "File name(s) containing solution(s) (*.hs)"))
  <*> switch
      (long "concurrently"
      <> short 'c'
      <> help "Execute concurrently")
  <*> switch  
      (long "markdown"
      <> help "Generate markdown report")

mkInitDbOptions :: Parser Command
mkInitDbOptions = pure InitDb 

mkGenTasklistOptions :: Parser Command
mkGenTasklistOptions = GenTasklist 
  <$> strArgument
      (metavar "TITLE"
      <> help "Title of tasklist")
  <*> option auto
      (long "specs"
      <> metavar "[(SCORE, N)..]"
      <> short 's'
      <> help "Specification of tasklist")
  <*> optional (strOption
      (long "description"
      <> short 'd'
      <> help "Description of tasklist"))

mkQueryTasksOptions :: Parser Command
mkQueryTasksOptions = pure QueryTasks

mkQueryTasklistsOptions :: Parser Command
mkQueryTasklistsOptions = pure QueryTasklists

mkQueryTasklistOptions :: Parser Command
mkQueryTasklistOptions = QueryTasklistTasks
  <$> strArgument
      (metavar "TITLE"
      <> help "Title of tasklist")
  <*> switch  
      (long "markdown"
      <> help "Generate markdown report")

mkGenSolutionOptions :: Parser Command
mkGenSolutionOptions = GenSolution
  <$> strArgument
      (metavar "TITLE"
      <> help "Title of tasklist")
  <*> strArgument
      (metavar "FILE" 
      <> help "File name containing generated solution (*.hs)")

cmdLineParser :: IO Command
cmdLineParser = execParser opts
  where
    opts = info (commandParser <**> helper)
                (fullDesc <> progDesc "Property-based testing application built on QuickCheck")