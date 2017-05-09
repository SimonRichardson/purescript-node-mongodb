module Test.Main where

import Prelude
import Data.Either (Either(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log) as Eff
import Control.Monad.Eff.Exception (EXCEPTION, Error)
import Control.Monad.Aff (Aff, launchAff, attempt)
import Control.Monad.Aff.Console (CONSOLE, log, logShow)
import Database.Mongo.Mongo (DB, Database, close, connect)
import Test.Database.Mongo.Insert as Insert
import Test.Database.Mongo.Find as Find
import Test.Database.Mongo.Update as Update
import Test.Assert (ASSERT)
import Test.Type (Test)

connectDB :: forall e. String -> Aff ( db :: DB | e) (Either Error Database)
connectDB dbUri =  attempt $ connect dbUri

testInsert :: Database -> Test Unit
testInsert = Insert.main

testFind :: Database -> Test Unit
testFind = Find.main

testUpdate :: Database -> Test Unit
testUpdate = Update.main

uri :: String
uri = "mongodb://127.0.0.1:27017/purescript-node-mongodb-test"

main :: Eff (console :: CONSOLE, db :: DB, exception :: EXCEPTION, assert :: ASSERT ) Unit
main = do
  Eff.log "Testing purescript-node-mongodb"
  void $ launchAff do
    log "connecting db"
    eitherDatabase <- connectDB uri
    case eitherDatabase of
      Left error -> logShow error
      Right database -> do
        testInsert database
        testFind database
        testUpdate database
        close database
