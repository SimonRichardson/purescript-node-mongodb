module Database.Mongo.Mongo
  ( 
  Client(), Database(), Collection(), Cursor()
  , AffDatabase(), AffCollection(), AffCursor()
  , AffResult(), AffUnit(), AffWriteResult()
  , Selector(), Fields()
  , connect, connect'
  , defaultDb, db, db'
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

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn5, Fn6, Fn7, Fn8, Fn9, runFn1, runFn2, runFn3, runFn5, runFn6, runFn7, runFn8, runFn9)
import Database.Mongo.Bson.BsonValue (Document, printBson)
import Database.Mongo.Options (InsertOptions, insertOptions, UpdateOptions, updateOptions)
import Database.Mongo.Results (WriteResult)
import Effect (Effect)
import Effect.Aff (Aff, Canceler, makeAff, nonCanceler)
import Effect.Exception (Error, error)
import Prelude (class Show, Unit, identity, pure, show, ($), (<<<))
import Text.Parsing.Parser (ParseError, runParser)
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.URIRef (Fragment, HierPath, Host, Path, Port, Query, RelPath, URIRefOptions, UserInfo, parser, print)


foreign import data Client :: Type
foreign import data Database :: Type
foreign import data Collection :: Type
foreign import data Cursor :: Type

type AffClient      = Aff Client
type AffDatabase    = Aff Database
type AffCollection  = Aff Collection
type AffCursor      = Aff Cursor
type AffResult a    = Aff a
type AffUnit        = Aff Unit
type AffWriteResult = Aff WriteResult

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
connect :: String -> AffClient
connect = makeAff <<< connect'

-- | Get default database from client 
defaultDb :: Client -> Database
defaultDb = runFn1 _defaultDb

-- | Get database from client by name and options
db :: String -> Json -> Client -> Database
db = runFn3 _db

-- | Get database from client by name 
db' :: String -> Client -> Database
db' = runFn2 __db

-- | Close the connection to the database
close :: Client -> AffUnit
close = makeAff <<< close'

-- | Get the collection
collection :: String -> Database -> AffCollection
collection a b = makeAff (collection' a b)

-- | Find one item in the collection
findOne :: forall a. (DecodeJson a) => Document -> Document -> Collection -> AffResult a
findOne s h c = makeAff $ findOne' s h c

-- | Find in the collection (essentially findMany)
find :: Document -> Document -> Collection -> AffCursor
find s h c = makeAff $ find' s h c

-- | Collect the results from the cursor
collect :: forall a. (DecodeJson a) => Cursor -> AffResult a
collect = makeAff <<< collect'

-- | Collect one result from the cursor
collectOne :: forall a. (DecodeJson a) => Cursor -> AffResult a
collectOne = makeAff <<< collectOne'

-- | Insert a new document using the selector, returning the write result
insertOne :: forall a. (EncodeJson a) => a -> InsertOptions -> Collection -> AffWriteResult
insertOne j o c = makeAff $ insertOne' j o c

-- | Insert new documents using the selector, returning the write result
insertMany :: forall a. (EncodeJson a) => a -> InsertOptions -> Collection -> AffWriteResult 
insertMany j o c = makeAff $ insertMany' j o c

-- | Update a new document using the selector, returning the write result
updateOne :: Document -> Fields -> UpdateOptions -> Collection -> AffWriteResult 
updateOne s j o c = makeAff $ updateOne' s j o c

-- | Update documents using the selector, returning the write result
updateMany :: Document -> Fields -> UpdateOptions -> Collection -> AffWriteResult 
updateMany s j o c = makeAff $ updateMany' s j o c

-- | Run a request directly without using 'Aff'
connect' :: String 
        -> (Either Error Client -> Effect Unit)
        -> Effect Canceler
connect' s cb = do
  case runParser s (parser uriOptions) of
    Left err -> 
      runFn3 _handleParseFailure (err' err) ignoreCancel eb
    Right x  -> 
      runFn5 _connect (print uriOptions x) ignoreCancel cb Left Right
  where
    err' :: ParseError -> Error
    err' e = error $ show e

    eb :: Error -> Effect Unit 
    eb = cb <<< Left

close' 
  :: Client
  -> (Either Error Unit -> Effect Unit)
  -> Effect Canceler
close' client cb = 
  runFn5 _close client ignoreCancel cb Left Right

collection' :: String
            -> Database
            -> (Either Error Collection -> Effect Unit)
            -> Effect Canceler
collection' name d cb = 
  runFn6 _collection name d ignoreCancel cb Left Right 

findOne' :: forall a. (DecodeJson a)
                  => Selector
                  -> Fields
                  -> Collection
                  -> (Either Error a -> Effect Unit)
                  -> Effect Canceler
findOne' s h c cb = 
  runFn7 _findOne (printBson s) (printBson h) c ignoreCancel cb' Left Right
  where
    cb' = decodeCallback cb

find' 
  :: Selector
  -> Fields
  -> Collection
  -> (Either Error Cursor -> Effect Unit)
  -> Effect Canceler
find' s h c cb = 
  runFn7 _find (printBson s) (printBson h) c ignoreCancel cb Left Right

collect' :: forall a. (DecodeJson a)
                    => Cursor
                    -> (Either Error a -> Effect Unit)
                    -> Effect Canceler
collect' c cb = 
  runFn5 _collect c ignoreCancel cb' Left Right
  where
    cb' = decodeCallback cb

collectOne' :: forall a
                . (DecodeJson a)
                => Cursor
                -> (Either Error a -> Effect Unit)
                -> Effect Canceler
collectOne' c cb = 
  runFn5 _collectOne c ignoreCancel cb' Left Right
  where
    cb' = decodeCallback cb 

insertOne' :: forall a
            . (EncodeJson a)
            => a
            -> InsertOptions
            -> Collection
            -> (Either Error WriteResult -> Effect Unit)
            -> Effect Canceler
insertOne' j o c cb = 
  runFn8 _insert fnTypeOne j' o' c ignoreCancel cb' Left Right
  where
    j' = encodeJson j
    o' = insertOptions o
    cb' = decodeCallback cb

insertMany' :: forall a
            . (EncodeJson a)
            => a
            -> InsertOptions
            -> Collection
            -> (Either Error WriteResult -> Effect Unit)
            -> Effect Canceler
insertMany' j o c cb = 
  runFn8 _insert fnTypeMany j' (insertOptions o) c ignoreCancel cb' Left Right
  where
    j' = encodeJson j
    o' = insertOptions o
    cb' = decodeCallback cb

updateOne' 
  :: Selector
  -> Fields
  -> UpdateOptions
  -> Collection
  -> (Either Error WriteResult -> Effect Unit)
  -> Effect Canceler
updateOne' s j o c cb = 
  runFn9 _update fnTypeOne (printBson s) j' o' c ignoreCancel cb' Left Right
  where
    j' = printBson j
    o' = updateOptions o
    cb' = decodeCallback cb

updateMany' 
  :: Selector
  -> Fields
  -> UpdateOptions
  -> Collection
  -> (Either Error WriteResult -> Effect Unit)
  -> Effect Canceler
updateMany' s j o c cb = 
  runFn9 _update fnTypeMany (printBson s) j' o' c ignoreCancel cb' Left Right
  where
    j' = printBson j
    o' = updateOptions o
    cb' = decodeCallback cb

uriOptions :: Record (URIRefOptions UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment)
uriOptions =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseRelPath: pure
  , printRelPath: identity
  , parseQuery: pure
  , printQuery: identity
  , parseFragment: pure
  , printFragment: identity
  }


decodeCallback 
  :: forall a 
  . (DecodeJson a)
  => (Either Error a -> Effect Unit) 
  -> Either Error Json 
  -> Effect Unit
decodeCallback cb val = cb $ case val of 
  Left err -> Left err
  Right res -> lmap (error <<< show) (decodeJson res) 
  
-- | Always ignore the cancel.
ignoreCancel :: forall a. a -> Canceler 
ignoreCancel _ = nonCanceler


-- | foreign imports
foreign import _connect :: Fn5
                   String
                   (Client -> Canceler)
                   (Either Error Client -> Effect Unit)
                   (Error -> Either Error Client)
                   (Client -> Either Error Client)
                   (Effect Canceler)

foreign import _defaultDb :: Fn1 Client Database
foreign import _db :: Fn3 String Json Client Database
foreign import __db :: Fn2 String Client Database

foreign import _handleParseFailure :: Fn3
                   Error
                   (Client -> Canceler)
                   (Error -> Effect Unit)
                   (Effect Canceler)

foreign import _close :: Fn5
                   Client
                   (Unit -> Canceler)
                   (Either Error Unit -> Effect Unit)
                   (Error -> Either Error Unit)
                   (Unit -> Either Error Unit)
                   (Effect Canceler)

foreign import _collection :: Fn6
                   String
                   Database
                   (Database -> Canceler)
                   (Either Error Collection -> Effect Unit)
                   (Error -> Either Error Collection)
                   (Collection -> Either Error Collection)
                   (Effect Canceler)

foreign import _collect :: Fn5
                   Cursor
                   (Cursor -> Canceler)
                   (Either Error Json -> Effect Unit)
                   (Error -> Either Error Json)
                   (Json -> Either Error Json)
                   (Effect Canceler)

foreign import _collectOne :: Fn5
                   Cursor
                   (Cursor -> Canceler)
                   (Either Error Json -> Effect Unit)
                   (Error -> Either Error Json)
                   (Json -> Either Error Json)
                   (Effect Canceler)

foreign import _findOne :: Fn7
                   Json
                   Json
                   Collection
                   (Collection -> Canceler)
                   (Either Error Json -> Effect Unit)
                   (Error -> Either Error Json)
                   (Json -> Either Error Json)
                   (Effect Canceler)

foreign import _find :: Fn7
                   Json
                   Json
                   Collection
                   (Collection -> Canceler)
                   (Either Error Cursor -> Effect Unit)
                   (Error -> Either Error Cursor)
                   (Cursor -> Either Error Cursor)
                   (Effect Canceler)

foreign import _insert :: Fn8
                   String
                   Json
                   Json
                   Collection
                   (Collection -> Canceler)
                   (Either Error Json -> Effect Unit)
                   (Error -> Either Error Json)
                   (Json -> Either Error Json)
                   (Effect Canceler)

foreign import _update :: Fn9
                   String
                   Json
                   Json
                   Json
                   Collection
                   (Collection -> Canceler)
                   (Either Error Json -> Effect Unit)
                   (Error -> Either Error Json)
                   (Json -> Either Error Json)
                   (Effect Canceler)
