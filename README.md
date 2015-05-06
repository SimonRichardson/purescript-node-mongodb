# purescript-node-mongodb

An library taking advantage of purescript-aff to enable pain-free asynchronous 
MongoDB queries.

## Getting Started

### Installation

```bash
bower install purescript-node-mongodb
```

### Introduction

You can construct queries with the `mongo` functions:

```purescript
main = launchAff $ do
  db <- attempt $ connect $ defaultOptions
  col <- collection "events" db
  evt <- findOne [ "name" := "Amazing!" ] [ "name" := 1 ] col

  liftEff $ traceAny (evt :: [Event])

  close db
```

See the module documentation for a [full list of these helpers](docs/Database.Mongo.Mongo.md#find)

### Module documentation

- [Database.Mongo.Mongo](docs/Database.Mongo.Mongo.md)
- [Database.Mongo.Options](docs/Database.Mongo.Options.md)
- [Database.Mongo.Results](docs/Database.Mongo.Results.md)
- [Database.Mongo.Bson.BsonValue](docs/Database.Mongo.Bson.BsonValue.md)

The following is likely to be removed in favor of [purescript-uri](https://github.com/slamdata/purescript-uri)

- [Database.Mongo.ConnectionInfo](docs/Database.Mongo.ConnectionInfo.md)

### General

Note: This is a wrapper around the _node_ library and as such is limited to that
very fact. It's possible to write a better interface (with more time) that 
removes the node library and talks directly to the database.