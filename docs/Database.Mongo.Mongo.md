# Module Documentation

## Module Database.Mongo.Mongo

#### `DB`

``` purescript
data DB :: !
```

The effect type for DB request made with Mongo

#### `Client`

``` purescript
data Client :: *
```


#### `Database`

``` purescript
data Database :: *
```


#### `Collection`

``` purescript
data Collection :: *
```


#### `Cursor`

``` purescript
data Cursor :: *
```


#### `AffDatabase`

``` purescript
type AffDatabase e = Aff (db :: DB | e) Database
```


#### `AffCollection`

``` purescript
type AffCollection e = Aff (db :: DB | e) Collection
```


#### `AffCursor`

``` purescript
type AffCursor e = Aff (db :: DB | e) Cursor
```


#### `AffResult`

``` purescript
type AffResult e a = Aff (db :: DB | e) a
```


#### `AffUnit`

``` purescript
type AffUnit e = Aff (db :: DB | e) Unit
```


#### `AffWriteResult`

``` purescript
type AffWriteResult e = Aff (db :: DB | e) WriteResult
```


#### `Selector`

``` purescript
type Selector = Document
```


#### `Fields`

``` purescript
type Fields = Document
```


#### `showMethodType`

``` purescript
instance showMethodType :: Show FnType
```


#### `connect`

``` purescript
connect :: forall e. ConnectionInfo -> AffDatabase e
```

Makes a connection to the database.

#### `close`

``` purescript
close :: forall e. Database -> AffUnit e
```

Close the connection to the database

#### `collection`

``` purescript
collection :: forall e. String -> Database -> AffCollection e
```

Get the collection

#### `findOne`

``` purescript
findOne :: forall e a. (DecodeJson a) => Document -> Document -> Collection -> AffResult e a
```

Find one item in the collection

#### `find`

``` purescript
find :: forall e. Document -> Document -> Collection -> AffCursor e
```

Find in the collection (essentially findMany)

#### `collect`

``` purescript
collect :: forall e a. (DecodeJson a) => Cursor -> AffResult e a
```

Collect the results from the cursor

#### `collectOne`

``` purescript
collectOne :: forall e a. (DecodeJson a) => Cursor -> AffResult e a
```

Collect one result from the cursor

#### `insertOne`

``` purescript
insertOne :: forall e a. (EncodeJson a) => a -> InsertOptions -> Collection -> AffWriteResult e
```

Insert a new document using the selector, returning the write result

#### `insertMany`

``` purescript
insertMany :: forall e a. (EncodeJson a) => a -> InsertOptions -> Collection -> AffWriteResult e
```

Insert new documents using the selector, returning the write result

#### `updateOne`

``` purescript
updateOne :: forall e. Document -> Fields -> UpdateOptions -> Collection -> AffWriteResult e
```

Update a new document using the selector, returning the write result

#### `updateMany`

``` purescript
updateMany :: forall e. Document -> Fields -> UpdateOptions -> Collection -> AffWriteResult e
```

Update documents using the selector, returning the write result

#### `connect'`

``` purescript
connect' :: forall e. ConnectionInfo -> (Error -> Eff (db :: DB | e) Unit) -> (Database -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```

Run a request directly without using 'Aff'

#### `close'`

``` purescript
close' :: forall e. Database -> (Error -> Eff (db :: DB | e) Unit) -> (Unit -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `collection'`

``` purescript
collection' :: forall e. String -> Database -> (Error -> Eff (db :: DB | e) Unit) -> (Collection -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `findOne'`

``` purescript
findOne' :: forall e a. (DecodeJson a) => Selector -> Fields -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (a -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `find'`

``` purescript
find' :: forall e. Selector -> Fields -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (Cursor -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `collect'`

``` purescript
collect' :: forall e a. (DecodeJson a) => Cursor -> (Error -> Eff (db :: DB | e) Unit) -> (a -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `collectOne'`

``` purescript
collectOne' :: forall e a. (DecodeJson a) => Cursor -> (Error -> Eff (db :: DB | e) Unit) -> (a -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `insertOne'`

``` purescript
insertOne' :: forall e a. (EncodeJson a) => a -> InsertOptions -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (WriteResult -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `insertMany'`

``` purescript
insertMany' :: forall e a. (EncodeJson a) => a -> InsertOptions -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (WriteResult -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `updateOne'`

``` purescript
updateOne' :: forall e. Selector -> Fields -> UpdateOptions -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (WriteResult -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `updateMany'`

``` purescript
updateMany' :: forall e. Selector -> Fields -> UpdateOptions -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (WriteResult -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```




