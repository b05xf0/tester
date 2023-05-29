module Eval.Property where

import Prelude hiding ( unwords, unlines, init, drop )

import Data.Text as T ( Text, pack, init, unlines ) 
import Data.List as L ( drop, head, init ) 
import Data.Aeson ( FromJSON, ToJSON )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import Language.Haskell.Interpreter
    ( GhcError(errMsg), InterpreterError(..) )
import Test.QuickCheck as QC (Result(..), isSuccess)

import qualified Data.Property as D

data Property =
  Property
    { name    :: Text
    , passed  :: Bool
    , result  :: Text
    , details :: Text
    , score   :: Int } deriving (Show, Generic)

instance FromJSON Property
instance ToJSON Property

mkEval :: D.Property -> Either InterpreterError QC.Result -> Property
mkEval prop er = Property name passed result details score
  where
    name = D.name prop
    (passed, result, details) = mkResult er
    score = if passed then D.score prop else 0

mkPassed :: Either InterpreterError QC.Result -> Bool
mkPassed er = case er of
  Right result -> QC.isSuccess result
  _            -> False

mkResult :: Either InterpreterError QC.Result -> (Bool, Text, Text)
mkResult er = case er of
  Right ( QC.Success
    { output = o } )          -> (True, "Successful test", otherResultDetails o)
  Right ( Failure
    { reason = r
    , failingTestCase = c
    , output = o } )          -> (False, "Failed test", failureDetails r c o)
  Right ( GaveUp
    { output = o } )          -> (False, "Given up", otherResultDetails o)
  Right ( NoExpectedFailure 
    { output = o } )          -> (False, "No expected failure", otherResultDetails o) 
  Left  ( WontCompile es )    -> (False, "Won't compile", "GhcError: " <> wontCompileDetails es)
  Left  ( NotAllowed m )      -> (False, "Not allowed", "NotAllowed: " <> T.pack m)
  Left  ( UnknownError m )    -> (False, "Unknown error", "UnknownError: " <> T.pack m)
  Left  ( GhcException m )    -> (False, "GHC exception", "GhcExeption: " <> T.pack m)
  where
    failureDetails r c o = case c of
      [counterExample, appliedTo] -> T.init $ T.unlines [T.pack r, T.pack appliedTo <> T.pack counterExample]
      (counterExample:_)          -> T.init $ T.unlines [T.pack r, T.pack counterExample]
      _                           -> T.init $ T.pack r
    otherResultDetails = T.pack . L.drop 4 . L.init
    wontCompileDetails = T.pack . errMsg . L.head
