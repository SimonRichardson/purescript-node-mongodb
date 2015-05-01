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
type AffResult e = Aff (db :: DB | e) Json
```


#### `AffUnit`

``` purescript
type AffUnit e = Aff (db :: DB | e) Unit
```


#### `AffWriteResult`

``` purescript
type AffWriteResult e = Aff (db :: DB | e) WriteResult
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

#### `find`

``` purescript
find :: forall e. Document -> Document -> Collection -> AffCursor e
```

Find in the collection

#### `findOne`

``` purescript
findOne :: forall e. Document -> Document -> Collection -> AffResult e
```

Find one item in the collection

#### `collect`

``` purescript
collect :: forall e. Cursor -> AffResult e
```

Collect the results from the cursor

#### `collectOne`

``` purescript
collectOne :: forall e. Cursor -> AffResult e
```

Collect one result from the cursor

#### `insertOne`

``` purescript
insertOne :: forall e. Json -> InsertOptions -> Collection -> AffWriteResult e
```

Insert a new document using the selector, returning the write result

#### `insertMany`

``` purescript
insertMany :: forall e. Json -> InsertOptions -> Collection -> AffWriteResult e
```

Insert a new document using the selector, returning the write result

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


#### `find'`

``` purescript
find' :: forall e. Foreign -> Foreign -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (Cursor -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `findOne'`

``` purescript
findOne' :: forall e. Foreign -> Foreign -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (Json -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `collect'`

``` purescript
collect' :: forall e. Cursor -> (Error -> Eff (db :: DB | e) Unit) -> (Json -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `collectOne'`

``` purescript
collectOne' :: forall e a. Cursor -> (Error -> Eff (db :: DB | e) Unit) -> (Json -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `insertOne'`

``` purescript
insertOne' :: forall e. Json -> InsertOptions -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (WriteResult -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```


#### `insertMany'`

``` purescript
insertMany' :: forall e. Json -> InsertOptions -> Collection -> (Error -> Eff (db :: DB | e) Unit) -> (WriteResult -> Eff (db :: DB | e) Unit) -> Eff (db :: DB | e) (Canceler (db :: DB | e))
```




