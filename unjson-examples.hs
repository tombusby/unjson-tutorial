{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson hiding (Options)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text as Text hiding (unpack)
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

prettyOptions :: Options
prettyOptions =  Options { pretty = True, indent = 4, nulls = False}

objectNoErrors :: Value
objectNoErrors = object
    [ "name" .= ("test" :: Text.Text)
    , "array_of_ints" .=
        [ toJSON (123 :: Int)
        , toJSON (321 :: Int)
        ]
    , "optional_bool" .= True
    ]

objectWithErrors :: Value
objectWithErrors = object
    [ "name" .= (666 :: Int)
    , "array_of_ints" .=
        [ toJSON ("unexpected string" :: Text.Text)
        , toJSON (321 :: Int)
        ]
    , "optional_bool" .= True
    ]

doparse :: Value -> Data.Unjson.Result Example
doparse obj = parse unjsonExample obj

main = do
    let Result val iss = doparse objectNoErrors
    let Result val_e iss_e = doparse objectWithErrors
    print $ exampleArray val   -- This shows the parsed Aeson AST.
    print iss_e                -- This will print any parse errors that occurred if any.
    putStrLn $ toBSunpack  val -- Turns an Aeson AST into a JSON string
    putStrLn $ toBSunpack' val -- Turns an Aeson AST into a JSON string (with pretty print)
        where
            toBSunpack = unpack . unjsonToByteStringLazy unjsonExample
            toBSunpack' = unpack . unjsonToByteStringLazy' prettyOptions unjsonExample
