module Test.Main where

import Prelude

import Data.Either (Either(..))
import Database.Mongo.Mongo (Client, Database, close, connect, defaultDb)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, attempt)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Exception (Error)
import Test.Database.Mongo.Find as Find
import Test.Database.Mongo.Insert as Insert
import Test.Database.Mongo.Update as Update
import Test.Type (Test)
 
client :: String -> Aff (Either Error Client)
client dbUri =  attempt $ connect dbUri

testInsert :: Database -> Test Unit
testInsert = Insert.main

testFind :: Database -> Test Unit
testFind = Find.main

testUpdate :: Database -> Test Unit
testUpdate = Update.main

uri :: String
uri = "mongodb://127.0.0.1:27017/purescript-node-mongodb-test"

main :: Effect Unit
main = do
  log "Testing purescript-node-mongodb"
  void $ launchAff do
    liftEffect $ log "connecting db" 
    eitherDatabase <- client uri
    case eitherDatabase of
      Left error -> liftEffect $ logShow error
      Right cl -> do
        let database = defaultDb cl
        testInsert database
        testFind database
        testUpdate database
        close cl
 