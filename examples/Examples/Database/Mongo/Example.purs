module Examples.Database.Mongo.Example where

import Database.Mongo.Mongo
import Database.Mongo.ConnectionInfo
import Database.Mongo.Bson.BsonValue

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception

import Data.Argonaut (printJson)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either
import Data.Event
import Data.Maybe
import Data.String.Regex

import Debug.Trace

foreign import traceAny
  """
  function traceAny(a){
    return function () {
      console.log(a);
      return {};
    };
  }
  """ :: forall e a. a -> Eff (trace :: Trace | e) Unit

main = launchAff $ do
  
  Right database <- attempt $ connect $ defaultOptions { db = Just "test" }
  col <- collection "events" database
  cur <- find [ 
                "$or" := [ "name" := (regex "Amazing" noFlags)
                         , "name" := (regex "Wow!" noFlags)
                         ]
              ] [] col
  res <- collect cur

  liftEff $ case decodeEvents res of
    Left err -> traceAny err
    Right x -> traceAny x
  
  close database


  where
    decodeEvents :: Json -> (Either String [Event])
    decodeEvents = decodeJson