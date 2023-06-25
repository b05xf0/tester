module Main where
import Parser
import Test.HUnit
import Data.Text
import qualified System.Exit as Exit
import Data

test1 :: Test
test1 = TestCase
  ( assertEqual
      ""
      [ "### Task1\nsometext1\n"
      , "### Task2\nsometext2\n"
      , "### Task3\nsometext3\n" ]
      (breaksOn "###" "### Task1\nsometext1\n### Task2\nsometext2\n### Task3\nsometext3") )

test2 :: Test
test2 = TestCase
  ( assertEqual
      ""
      (Right (mkProperty ("prop_countDown1", "countDown", "prop_countDown1 n = n > 0 ==> last (countDown n) == Alarm", 1)))
      (parseProp "countDown" "-- prop_countDown1 (1)\nprop_countDown1 n = n > 0 ==> last (countDown n) == Alarm" ) )

test3 :: Test
test3 = TestCase
  ( assertEqual
      ""
      (Left "invalid property score")
      (parseProp "countDown" "-- prop_countDown1\nprop_countDown1 n = n > 0 ==> last (countDown n) == Alarm" ) )

test4 :: Test
test4 = TestCase
  ( assertEqual
      ""
      (Left "missing property source")
      (parseProp "countDown" "-- prop_countDown1 (1)" ) )


test5_input :: Text
test5_input = "### `countDown` (4)\n\nA `Clock` adattípus egy ketyegő óra állapotait valósítja meg. Írj függvényt amely ezt a típust felhasználva a függvény paramétertől elszámol 0-ig (felépít egy listát), úgy hogy a páros számokat a `Tick`, míg a páratlanokat a `Tock` tárolja, végül a 0 helyén `Alarm` legyen! Tipp: használhatsz rekurziót, de akár `map` függvénnyel is megoldható a feladat.\n```haskell\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\ncountDown :: Int -> [Clock]\n\n-- SOLUTION\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\n\ncountDown :: Int -> [Clock]\ncountDown x\n  | x <= 0 = [Alarm]\n  | even x = Tick x : countDown (x - 1)\n  | odd x  = Tock x : countDown (x - 1)\n-- prop_countDown1 (1)\nprop_countDown1 n = n > 0 ==> last (countDown n) == Alarm\n```"

test5_output_props :: [Property]
test5_output_props = [mkProperty ("prop_countDown1", "countDown", "prop_countDown1 n = n > 0 ==> last (countDown n) == Alarm", 1)]

test5_output_task :: Task
test5_output_task = mkTask 
  ( "countDown"
  , "A `Clock` adattípus egy ketyegő óra állapotait valósítja meg. Írj függvényt amely ezt a típust felhasználva a függvény paramétertől elszámol 0-ig (felépít egy listát), úgy hogy a páros számokat a `Tick`, míg a páratlanokat a `Tock` tárolja, végül a 0 helyén `Alarm` legyen! Tipp: használhatsz rekurziót, de akár `map` függvénnyel is megoldható a feladat."
  , "data Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\ncountDown :: Int -> [Clock]"
  , "data Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\n\ncountDown :: Int -> [Clock]\ncountDown x\n  | x <= 0 = [Alarm]\n  | even x = Tick x : countDown (x - 1)\n  | odd x  = Tock x : countDown (x - 1)"
  , 4 )

test5_output :: (Task, [Property])
test5_output =  (test5_output_task, test5_output_props)

test5 :: Test
test5 = TestCase
  ( assertEqual
      ""
      (Right test5_output)
      (parseTask test5_input) )

test6_input :: Text
test6_input = "### `countDown` (4)\n\nA `Clock` adattípus egy ketyegő óra állapotait valósítja meg. Írj függvényt amely ezt a típust felhasználva a függvény paramétertől elszámol 0-ig (felépít egy listát), úgy hogy a páros számokat a `Tick`, míg a páratlanokat a `Tock` tárolja, végül a 0 helyén `Alarm` legyen! Tipp: használhatsz rekurziót, de akár `map` függvénnyel is megoldható a feladat.\n```haskell\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\ncountDown :: Int -> [Clock]\n\n-- SOLUTION\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\n\ncountDown :: Int -> [Clock]\ncountDown x\n  | x <= 0 = [Alarm]\n  | even x = Tick x : countDown (x - 1)\n  | odd x  = Tock x : countDown (x - 1)\n-- prop_countDown1\nprop_countDown1 n = n > 0 ==> last (countDown n) == Alarm\n```"

test6_output :: Text
test6_output = "Invalid property: invalid property score"

test6 :: Test
test6 = TestCase
  ( assertEqual
      ""
      (Left test6_output)
      (parseTask test6_input) )

test7_input :: Text
test7_input = "### `countDown`\n\nA `Clock` adattípus egy ketyegő óra állapotait valósítja meg. Írj függvényt amely ezt a típust felhasználva a függvény paramétertől elszámol 0-ig (felépít egy listát), úgy hogy a páros számokat a `Tick`, míg a páratlanokat a `Tock` tárolja, végül a 0 helyén `Alarm` legyen! Tipp: használhatsz rekurziót, de akár `map` függvénnyel is megoldható a feladat.\n```haskell\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\ncountDown :: Int -> [Clock]\n\n-- SOLUTION\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\n\ncountDown :: Int -> [Clock]\ncountDown x\n  | x <= 0 = [Alarm]\n  | even x = Tick x : countDown (x - 1)\n  | odd x  = Tock x : countDown (x - 1)\n-- prop_countDown1 (1)\nprop_countDown1 n = n > 0 ==> last (countDown n) == Alarm\n```"

test7_output :: Text
test7_output = "Invalid task score: ### `countDown`"

test7 :: Test
test7 = TestCase
  ( assertEqual
      ""
      (Left test7_output)
      (parseTask test7_input) )

test8_input :: Text
test8_input = "### `countDown (4)`\n\nA `Clock` adattípus egy ketyegő óra állapotait valósítja meg. Írj függvényt amely ezt a típust felhasználva a függvény paramétertől elszámol 0-ig (felépít egy listát), úgy hogy a páros számokat a `Tick`, míg a páratlanokat a `Tock` tárolja, végül a 0 helyén `Alarm` legyen! Tipp: használhatsz rekurziót, de akár `map` függvénnyel is megoldható a feladat.\n```haskell\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\ncountDown :: Int -> [Clock]\n\n-- SOLUTION\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\n\ncountDown :: Int -> [Clock]\ncountDown x\n  | x <= 0 = [Alarm]\n  | even x = Tick x : countDown (x - 1)\n  | odd x  = Tock x : countDown (x - 1)\n```"

test8_output :: Text
test8_output = "Missing properties: ```haskell\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\ncountDown :: Int -> [Clock]\n\n-- SOLUTION\ndata Clock = Tick Int | Tock Int | Alarm deriving (Eq, Show)\n\ncountDown :: Int -> [Clock]\ncountDown x\n  | x <= 0 = [Alarm]\n  | even x = Tick x : countDown (x - 1)\n  | odd x  = Tock x : countDown (x - 1)\n```\n"

test8 :: Test
test8 = TestCase
  ( assertEqual
      ""
      (Left test8_output)
      (parseTask test8_input) )

tests :: Test
tests = TestList
  [ TestLabel "breaksOn"           test1
  , TestLabel "parseProp success"  test2
  , TestLabel "parseProp failure1" test3
  , TestLabel "parseProp failure2" test4 
  , TestLabel "parseTask success"  test5 
  , TestLabel "parseTask failure1" test6 
  , TestLabel "parseTask failure2" test7 
  , TestLabel "parseTask failure3" test8 ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess