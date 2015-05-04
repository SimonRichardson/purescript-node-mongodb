module Database.Mongo.Mongo 
  ( DB()
  , Client(), Database(), Collection(), Cursor()
  , AffDatabase(), AffCollection(), AffCursor()
  , AffResult(), AffUnit(), AffWriteResult()
  , Selector(), Fields()
  , connect, connect'
  , close, close'
  , collection, collection'
  , find, find'
  , findOne, findOne'
  , collect, collect'
  , collectOne, collectOne'
  , insertOne, insertOne'
  , insertMany, insertMany'
  , updateOne, updateOne'
  , updateMany, updateMany'
  ) where

import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject)
import Data.Argonaut.Core (Json())
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either
import Data.Function (Fn4(), runFn4, Fn5(), runFn5, Fn6(), runFn6, Fn7(), runFn7, Fn8(), runFn8)
import Data.Maybe

import Database.Mongo.ConnectionInfo
import Database.Mongo.Options (InsertOptions(), insertOptions, UpdateOptions(), updateOptions)
import Database.Mongo.Results (WriteResult())
import Database.Mongo.Updatable (Updatable(..), printUpdatable)
import Database.Mongo.Bson.BsonValue (Document(..), printBson)

-- | The effect type for DB request made with Mongo
foreign import data DB :: !

foreign import data Client :: *
foreign import data Database :: *
foreign import data Collection :: *
foreign import data Cursor :: *

type AffDatabase e    = Aff (db :: DB | e) Database
type AffCollection e  = Aff (db :: DB | e) Collection
type AffCursor e      = Aff (db :: DB | e) Cursor 
type AffResult e      = Aff (db :: DB | e) Json
type AffUnit e        = Aff (db :: DB | e) Unit
type AffWriteResult e = Aff (db :: DB | e) WriteResult

type Selector = Document
type Fields   = Document

data FnType
  = One
  | Many

fnTypeOne :: String
fnTypeOne = show One

fnTypeMany :: String
fnTypeMany = show Many

instance showMethodType :: Show FnType where
  show One  = "One"
  show Many = "Many"

-- | Makes a connection to the database.
connect :: forall e. ConnectionInfo -> AffDatabase e
connect = makeAff' <<< connect'

-- | Close the connection to the database
close :: forall e. Database -> AffUnit e
close = makeAff' <<< close'

-- | Get the collection
collection :: forall e. String -> Database -> AffCollection e
collection a b = makeAff' (collection' a b)

-- | Find one item in the collection
findOne :: forall e. Document -> Document -> Collection -> AffResult e
findOne s h c = makeAff' $ findOne' s h c

-- | Find in the collection (essentially findMany)
find :: forall e. Document -> Document -> Collection -> AffCursor e
find s h c = makeAff' $ find' s h c

-- | Collect the results from the cursor
collect :: forall e. Cursor -> AffResult e
collect = makeAff' <<< collect'

-- | Collect one result from the cursor
collectOne :: forall e. Cursor -> AffResult e
collectOne = makeAff' <<< collectOne'

-- | Insert a new document using the selector, returning the write result
insertOne :: forall e a. (EncodeJson a) => a -> InsertOptions -> Collection -> AffWriteResult e
insertOne j o c = makeAff' $ insertOne' j o c

-- | Insert new documents using the selector, returning the write result
insertMany :: forall e a. (EncodeJson a) => a -> InsertOptions -> Collection -> AffWriteResult e
insertMany j o c = makeAff' $ insertMany' j o c

-- | Update a new document using the selector, returning the write result
updateOne :: forall e a. (EncodeJson a) => Document -> a -> UpdateOptions -> Collection -> AffWriteResult e
updateOne s j o c = makeAff' $ updateOne' s j o c

-- | Update documents using the selector, returning the write result
updateMany :: forall e a. (EncodeJson a) => Document -> a -> UpdateOptions -> Collection -> AffWriteResult e
updateMany s j o c = makeAff' $ updateMany' s j o c

-- | Run a request directly without using 'Aff'
connect' :: forall e
  .  ConnectionInfo
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Database -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
connect' info eb cb = runFn4 _connect (dialUri info) ignoreCancel eb cb

close' :: forall e
  .  Database
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Unit -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
close' database eb cb = runFn4 _close database ignoreCancel eb cb

collection' :: forall e
  .  String
  -> Database
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Collection -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
collection' name d eb cb = runFn5 _collection name d ignoreCancel eb cb

findOne' :: forall e
  .  Selector
  -> Fields
  -> Collection
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Json -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
findOne' s h c eb cb = runFn6 _findOne (printBson s) (printBson h) c ignoreCancel eb cb

find' :: forall e
  .  Selector
  -> Fields
  -> Collection
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Cursor -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
find' s h c eb cb = runFn6 _find (printBson s) (printBson h) c ignoreCancel eb cb

collect' :: forall e
  .  Cursor
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Json -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
collect' c eb cb = runFn4 _collect c ignoreCancel eb cb

collectOne' :: forall e a
  .  Cursor
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Json -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
collectOne' c eb cb = runFn4 _collectOne c ignoreCancel eb cb

insertOne' :: forall e a
  .  (EncodeJson a)
  => a
  -> InsertOptions
  -> Collection
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (WriteResult -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
insertOne' j o c eb cb = runFn7 _insert fnTypeOne j' o' c ignoreCancel eb cb'
  where
    j' = encodeJson j
    o' = insertOptions o
    cb' :: Json -> Eff (db :: DB | e) Unit
    cb' res = case decodeJson res of
      Left err   -> eb $ error (show err)
      Right res' -> cb res'

insertMany' :: forall e a
  .  (EncodeJson a)
  => a
  -> InsertOptions
  -> Collection
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (WriteResult -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
insertMany' j o c eb cb = runFn7 _insert fnTypeMany j' (insertOptions o) c ignoreCancel eb cb'
  where
    j' = encodeJson j
    o' = insertOptions o
    cb' :: Json -> Eff (db :: DB | e) Unit
    cb' res = case decodeJson res of
      Left err   -> eb $ error (show err)
      Right res' -> cb res'

updateOne' :: forall e a
  .  (EncodeJson a)
  => Selector
  -> a
  -> UpdateOptions
  -> Collection
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (WriteResult -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
updateOne' s j o c eb cb = runFn8 _update fnTypeOne (printBson s) j' o' c ignoreCancel eb cb'
  where
    j' = printUpdatable (URaw $ encodeJson j)
    o' = updateOptions o
    cb' :: Json -> Eff (db :: DB | e) Unit
    cb' res = case decodeJson res of
      Left err   -> eb $ error (show err)
      Right res' -> cb res'

updateMany' :: forall e a
  .  (EncodeJson a)
  => Selector
  -> a
  -> UpdateOptions
  -> Collection
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (WriteResult -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
updateMany' s j o c eb cb = runFn8 _update fnTypeMany (printBson s) j' o' c ignoreCancel eb cb'
  where
    j' = printUpdatable (USet $ encodeJson j)
    o' = updateOptions o
    cb' :: Json -> Eff (db :: DB | e) Unit
    cb' res = case decodeJson res of
      Left err   -> eb $ error (show err)
      Right res' -> cb res'      

-- | Always ignore the cancel.
ignoreCancel :: forall e a. a -> Canceler (db :: DB | e)
ignoreCancel c = Canceler \err -> makeAff (\eb cb -> runFn4 _ignoreCancel c err eb cb)

-- | foreign imports
foreign import _connect
  """
  function _connect(uri, canceler, errback, callback) {
    var client = require('mongodb').MongoClient;
    client.connect(uri, function(err, x) {
      (err ? errback(err) : callback(x))();
    });
    return canceler(client);
  }
  """ :: forall e. Fn4 
                   Uri
                   (Client -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Database -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _close
  """
  function _close(db, canceler, errback, callback) {
    db.close(function(err, x) {
      (err ? errback(err) : callback(x))();
    });
    return canceler({});
  }
  """ :: forall e. Fn4 
                   Database
                   (Unit -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Unit -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _collection
  """
  function _collection(name, db, canceler, errback, callback) {
    db.collection(name, function(err, x) {
      (err ? errback(err) : callback(x))();
    });
    return canceler(db);
  }
  """ :: forall e. Fn5 
                   String 
                   Database
                   (Database -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Collection -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _collect
  """
  function _collect(cursor, canceler, errback, callback) {
    cursor.toArray(function(err, x) {
      (err ? errback(err) : callback(x))();
    });
    return canceler(cursor);
  }
  """ :: forall e. Fn4
                   Cursor
                   (Cursor -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Json -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _collectOne
  """
  function _collectOne(cursor, canceler, errback, callback) {
    cursor.next(function(err, x) {
      if (err) {
        errback(err)();
      } else if (x === null) {
        var error = new Error('Not Found.');
        error.name = 'MongoError';
        errback(error)();
      } else {
        callback(x)();
      }      
    });
    return canceler(cursor);
  }
  """ :: forall e. Fn4
                   Cursor
                   (Cursor -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Json -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _findOne
  """
  function _findOne(selector, fields, collection, canceler, errback, callback) {
    collection.findOne(selector, fields, function(err, x) {
      (err ? errback(err) : callback(x))();
    });
    return canceler(collection);
  }
  """ :: forall e. Fn6
                   Json
                   Json
                   Collection
                   (Collection -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Json -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _find
  """
  function _find(selector, fields, collection, canceler, errback, callback) {
    collection.find(selector, fields, function(err, x) {
      (err ? errback(err) : callback(x))();
    });
    return canceler(collection);
  }
  """ :: forall e. Fn6
                   Json
                   Json
                   Collection
                   (Collection -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Cursor -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))                   

foreign import _insert
  """
  function _insert(type, json, options, collection, canceler, errback, callback) {
    collection["insert" + type](json, options, function(err, x) {
      (err ? errback(err) : callback(x.result))();
    });
    return canceler(collection);
  }
  """ :: forall e. Fn7
                   String
                   Json
                   Json
                   Collection
                   (Collection -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Json -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _update
  """
  function _update(type, selector, json, options, collection, canceler, errback, callback) {
    collection["update" + type](selector, json, options, function(err, x) {
      (err ? errback(err) : callback(x.result))();
    });
    return canceler(collection);
  }
  """ :: forall e. Fn8
                   String
                   Json
                   Json
                   Json
                   Collection
                   (Collection -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Json -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _ignoreCancel
  """
  function _ignoreCancel(any, cancelError, errback, callback) {
    return function() {
        return callback(false)();
    };
  }
  """ :: forall e a. Fn4 
                     a
                     Error
                     (Error -> Eff (db :: DB | e) Unit)
                     (Boolean -> Eff (db :: DB | e) Unit)
                     (Eff (db :: DB | e) Unit)
