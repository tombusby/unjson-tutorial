{-# LANGUAGE OverloadedStrings #-}

import Data.Text as Text
import Data.Unjson

data Example = Example
   { exampleName     :: Text.Text,
     exampleArray    :: [Int],
     exampleOptional :: Maybe Bool }

unjsonExample :: UnjsonDef Example
unjsonExample = objectOf $ pure Example
  <*> field "name"
          exampleName
          "Name used for example"
  <*> fieldDefBy "array_of_ints" []
          exampleArray
          "Array of integers, optional, defaults to empty list"
          (arrayOf unjsonDef)
  <*> fieldOpt "optional_bool"
          exampleOptional
          "Optional boolean"

main = do
    putStrLn "hello"
