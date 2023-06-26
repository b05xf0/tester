{-
Sources:

https://hackage.haskell.org/package/QuickCheck
https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html
https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html
https://www.dcc.fc.up.pt/~pbv/aulas/tapf/handouts/quickcheck.html
https://www.fpcomplete.com/blog/2017/01/quickcheck/
https://dl.acm.org/doi/10.1145/357766.351266

https://hackage.haskell.org/package/hint
https://hackage.haskell.org/package/hint-0.9.0.6/docs/Language-Haskell-Interpreter.html
https://kseo.github.io/posts/2017-01-19-fun-with-hint.html

https://hackage.haskell.org/package/async-2.2.4/docs/Control-Concurrent-Async.html
-}

module Eval ( runTests ) where

import Prelude hiding ( writeFile )

import System.FilePath ( (</>) )
import System.Directory ( removeFile )
import Data.Text ( unpack, Text ) 
import Data.Text.IO ( writeFile ) 
import Test.QuickCheck ( Result ) 
import Language.Haskell.Interpreter
    ( InterpreterError, Interpreter
    , loadModules, setImports, setTopLevelModules, as, interpret, runInterpreter )

import Control.Concurrent.Async ( mapConcurrently )

import Eval.Solution as ES ( mkEval )
import Eval.Task as ET ( mkEval )
import Eval.Property as EP ( mkEval )

import Eval.Solution as E ( Solution )
import Eval.Task as E ( Task )
import Eval.Property as E ( Property )

import Data as D
    ( Solution, Property, Task
    , getProps, getPropName, getPropSource, getSolSource, getSolIdx )

runQuickCheck :: [String] -> String -> Interpreter (IO Result)
runQuickCheck ms propName  = do
  loadModules ms
  setTopLevelModules ["Prop"]
  setImports ["Test.QuickCheck"]
  interpret ("quickCheckWithResult (stdArgs {chatty = False}) " <> propName) (as :: IO Result)

transformResult :: Either InterpreterError (IO Result) -> IO (Either InterpreterError Result)
transformResult r = case r of
  (Left error)   -> return $ Left error
  (Right result) -> Right <$> result  

runTests :: Bool -> [D.Task] -> [D.Solution] -> IO [E.Solution]
runTests concurrently tasks = distribute runOnSolution
  where
    distribute :: (a -> IO b) -> [a] -> IO [b]
    distribute = if concurrently then mapConcurrently else mapM
    
    runOnSolution :: D.Solution -> IO E.Solution
    runOnSolution sol = do 
      writeFile solModulePath srcSol
      taskEvals <- mapM runOnTask tasks
      removeFile solModulePath
      return $ ES.mkEval sol taskEvals
      where    
        sIdx = D.getSolIdx sol 
        solModulePath = buildSolModulePath sIdx 
        srcSol = "module Sol where\n\n" <> D.getSolSource sol

        runOnTask :: D.Task -> IO E.Task
        runOnTask t = do
          ps        <- snd <$> D.getProps t
          propEvals <- mapM runWithProperty ps
          return $ ET.mkEval t propEvals

        runWithProperty :: D.Property -> IO E.Property
        runWithProperty p = do
          writeFile propModulePath srcProp
          fromInterpreter <- runInterpreter quickCheckWithProp
          result          <- transformResult fromInterpreter
          removeFile propModulePath
          return $ EP.mkEval p result
          where
            quickCheckWithProp = runQuickCheck m $ unpack $ D.getPropName p
            srcProp = "module Prop where\n\nimport Sol\nimport Test.QuickCheck\n\n" <> D.getPropSource p        
            m@[_, propModulePath] = [solModulePath, buildPropModulePath sIdx $ D.getPropName p]

buildSolModulePath ::Int -> FilePath
buildSolModulePath sIdx = "_sol_" <> show sIdx <> ".hs"

buildPropModulePath :: Int  -> Text -> FilePath
buildPropModulePath sIdx pName = "_sol_" <> show sIdx <> "_" <> unpack pName <> ".hs"
