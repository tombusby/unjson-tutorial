{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
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

doparse :: Data.Unjson.Result Example
doparse = parse unjsonExample $ object 
    [ "name" .= ("test" :: Text.Text)
    , "array_of_ints" .= 
        [ toJSON (123 :: Int)
        , toJSON (321 :: Int)
        ]
    , "optional_bool" .= True
    ]

main = do
    let Result val iss = doparse
    print $ exampleArray val  -- This shows the parsed Aeson AST.
    print iss                 -- This will print any parse errors that occurred if any.
    print $ unjsonToByteStringLazy unjsonExample val -- Turns an Aeson AST into a JSON string
