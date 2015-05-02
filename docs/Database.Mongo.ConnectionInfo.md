# Module Documentation

## Module Database.Mongo.ConnectionInfo

#### `ConnectionInfo`

``` purescript
type ConnectionInfo = { password :: Maybe Password, user :: Maybe User, db :: Maybe Db, port :: Maybe Port, host :: Host }
```

The type for Mongo connection options

#### `defaultOptions`

``` purescript
defaultOptions :: ConnectionInfo
```

Standard default options for connecting to mongodb.

#### `Uri`

``` purescript
type Uri = String
```

Type alias for URI connections to aid readability of types.

#### `Host`

``` purescript
type Host = String
```


#### `Port`

``` purescript
type Port = Number
```


#### `Db`

``` purescript
type Db = String
```


#### `User`

``` purescript
type User = String
```


#### `Password`

``` purescript
type Password = String
```


#### `dialUri`

``` purescript
dialUri :: ConnectionInfo -> Uri
```


#### `showHost`

``` purescript
showHost :: Host -> String
```


#### `showPort`

``` purescript
showPort :: Port -> String
```


#### `showDb`

``` purescript
showDb :: Db -> String
```


#### `showUser`

``` purescript
showUser :: User -> String
```


#### `showPassword`

``` purescript
showPassword :: Password -> String
```




