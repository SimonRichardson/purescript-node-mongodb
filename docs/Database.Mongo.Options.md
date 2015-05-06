# Module Documentation

## Module Database.Mongo.Options

#### `WriteConcern`

``` purescript
type WriteConcern = Number
```

The type of WriteConcern

#### `InsertOptions`

``` purescript
newtype InsertOptions
```

Typed options for inserting documents into a collection.

#### `defaultInsertOptions`

``` purescript
defaultInsertOptions :: InsertOptions
```


#### `insertOptions`

``` purescript
insertOptions :: InsertOptions -> Json
```


#### `decodeJsonInsertOptions`

``` purescript
instance decodeJsonInsertOptions :: DecodeJson InsertOptions
```


#### `encodeJsonInsertOptions`

``` purescript
instance encodeJsonInsertOptions :: EncodeJson InsertOptions
```


#### `UpdateOptions`

``` purescript
newtype UpdateOptions
```

Typed options for updating documents into a collection.

#### `defaultUpdateOptions`

``` purescript
defaultUpdateOptions :: UpdateOptions
```


#### `updateOptions`

``` purescript
updateOptions :: UpdateOptions -> Json
```


#### `decodeJsonUpdateOptions`

``` purescript
instance decodeJsonUpdateOptions :: DecodeJson UpdateOptions
```


#### `encodeJsonUpdateOptions`

``` purescript
instance encodeJsonUpdateOptions :: EncodeJson UpdateOptions
```




