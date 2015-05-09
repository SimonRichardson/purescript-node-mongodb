module Examples.Database.Mongo.Update where

import Database.Mongo.Mongo
import Database.Mongo.Options
import Database.Mongo.Bson.BsonValue

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception

import Data.Argonaut (printJson)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Either
import Data.Event
import Data.Maybe
import Data.String.Regex
import Data.URI

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

evt :: Event
evt = Event
  { name : Just "Wow!"
  }

uri :: String
uri = "mongodb://127.0.0.1/events"

main = launchAff $ do
  
  Right database <- attempt $ connect uri
  col <- collection "events" database
  res <- updateMany [] ["$set" := encodeJson evt] defaultUpdateOptions col

  liftEff $ traceAny res
  
  close database
