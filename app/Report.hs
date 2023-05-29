module Report where

import Data.Aeson ( FromJSON, ToJSON (toJSON), object, KeyValue ((.=)), Value )
import Data.Aeson.Encode.Pretty ( encodePretty )

import Data.ByteString.Lazy as B ( ByteString )

import Prelude hiding (unlines, replicate, unwords)

import qualified Eval.Solution as ES
import qualified Eval.Task as ET
import qualified Eval.Property as EP

import qualified Data.Solution as DS
import qualified Data.Task as DT
import qualified Data.Tasklist as DTL
import qualified Data.Property as DP

import Data.Text as T ( Text, replicate, unlines, unwords, init, pack)
import GHC.Generics (Generic)

taskSolutionsToHs :: [DT.Task] -> Text
taskSolutionsToHs ts = unlines $ imports <> solutions
  where
    imports   = ("import " <>) <$> [ "Data.Char", "Data.List", "Data.Foldable"]
    solutions = DT.solution <$> ts

data EvalReport =
  EvalReport
    { title     :: Text
    , solutions :: [ES.Solution] } deriving (Show, Generic)

instance FromJSON EvalReport
instance ToJSON EvalReport

evalToMd :: EvalReport -> Text
evalToMd r = mdH 1 (title r) <> sols
  where
    mdSol s  = mdH 2 ( unwords
        [ ES.fname s
        , pack (show $ ES.score s) <> "/" <> pack (show $ ES.maxScore s) ] )
      <> tasks s

    mdTask t = mdH 3 ( unwords
        [ mdCodeI $ ET.name t
        , pack (show $ ET.score t) <> "/" <> pack (show $ ET.maxScore t) ] )
      <> props t

    mdProp p = T.init $ mdP (unwords
        [ mdCodeI $ EP.name p
        , EP.result p ] )
      <> mdCodeB (EP.details p)

    sols    = T.init . unlines $ mdSol <$> solutions r
    tasks s = T.init . unlines $ mdTask <$> ES.tasks s
    props t = T.init . unlines $ mdProp <$> ET.props t

data TaskReport =
  TaskReport
    { task  :: DT.Task
    , props :: [DP.Property]
    } deriving (Show, Generic)

instance FromJSON TaskReport
instance ToJSON TaskReport

mkTaskReport :: (DT.Task, [DP.Property]) -> TaskReport
mkTaskReport (task, props) = TaskReport task props

type TasklistReport = (Maybe DTL.Tasklist, [TaskReport])

newtype ToSay = ToSay (Either Text Text)

instance ToJSON ToSay where
    toJSON :: ToSay -> Value
    toJSON (ToSay (Left l))  = object ["error" .= l]
    toJSON (ToSay (Right r)) = object ["result" .= r]


toJson :: ToJSON a => a -> ByteString
toJson = encodePretty

tasklistToMd :: TasklistReport -> Text
tasklistToMd (Nothing, rt) = ""
tasklistToMd (Just tl, rt)  
  =  mdH 1 (DTL.title tl)
  <> mdP (DTL.description tl)
  <> unlines (mdTask . task <$> rt)
  where
    mdTask t = mdH 3 ( unwords
        [ mdCodeI (DT.name t)
        ,"(" <> pack (show $ DT.score t) <> ")" ] )
      <> mdP (DT.description t)
      <> mdCodeBH (DT.signature t)


mdH :: Int -> Text -> Text
mdH n t = T.replicate n "#" <> " " <> t <> mdBr

mdCodeBH :: Text -> Text
mdCodeBH t = "```haskell\n" <> t <> "\n```" <> mdBr

mdCodeB :: Text -> Text
mdCodeB t = "```\n" <> t <> "\n```" <> mdBr

mdCodeI :: Text -> Text
mdCodeI t = "`" <> t <> "`"

mdStrong :: Text -> Text
mdStrong t = "**" <> t <> "**"

mdEm :: Text -> Text
mdEm t = "*" <> t <> "*"

mdStrongEm :: Text -> Text
mdStrongEm t = "***" <> t <> "***"

mdP :: Text -> Text
mdP t = t <> mdBr

mdLi :: Text -> Text
mdLi t = "- " <> t <> mdBr

mdBr :: Text
mdBr = "\n\n"