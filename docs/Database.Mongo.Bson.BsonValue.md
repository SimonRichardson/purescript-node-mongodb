# Module Documentation

## Module Database.Mongo.Bson.BsonValue

#### `Field`

``` purescript
type Field = Tuple String BsonValue
```


#### `Document`

``` purescript
type Document = [Field]
```


#### `ObjectId`

``` purescript
data ObjectId
  = ObjectId String
```


#### `BsonValue`

``` purescript
data BsonValue
  = VString String
  | VNumber Number
  | VRegex Regex
  | VDocument [Tuple String BsonValue]
  | VArray [BsonValue]
  | VObjectId ObjectId
  | VJson Json
```


#### `IsBsonValue`

``` purescript
class IsBsonValue a where
  toBson :: a -> BsonValue
```


#### `stringBson`

``` purescript
instance stringBson :: IsBsonValue String
```


#### `numberBson`

``` purescript
instance numberBson :: IsBsonValue Number
```


#### `regexBson`

``` purescript
instance regexBson :: IsBsonValue Regex
```


#### `documentBson`

``` purescript
instance documentBson :: IsBsonValue [Tuple String BsonValue]
```


#### `arrayBson`

``` purescript
instance arrayBson :: IsBsonValue [BsonValue]
```


#### `objectIdBson`

``` purescript
instance objectIdBson :: IsBsonValue ObjectId
```


#### `jsonBson`

``` purescript
instance jsonBson :: IsBsonValue Json
```


#### `(:=)`

``` purescript
(:=) :: forall a. (IsBsonValue a) => String -> a -> Tuple String BsonValue
```


#### `printBson`

``` purescript
printBson :: Document -> Json
```




