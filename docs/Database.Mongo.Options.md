# Module Documentation

## Module Database.Mongo.Options

#### `InsertOptions`

``` purescript
newtype InsertOptions
```


#### `defaultInsertOptions`

``` purescript
defaultInsertOptions :: InsertOptions
```


#### `WriteConcern`

``` purescript
type WriteConcern = Number
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




