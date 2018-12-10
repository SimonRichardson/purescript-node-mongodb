// module Database.Mongo.Mongo

'use strict';

exports._connect = function _connect(uri, canceler, callback, left, right) {
  var client = require('mongodb').MongoClient;
  client.connect(uri, { useNewUrlParser: true }, function(err, x) {
    
    if (err) {
      return callback(left(err))();
    }

    return callback(right(x))();
  });

  return canceler(client);
};

exports._defaultDb = function _defaultDb(client) {
  return client.db();
}

exports._db = function _defaultDb(dbName, options, client) {
  return client.db(dbName, options);
}

exports.__db = function _defaultDb(dbName, client) {
  return client.db(dbName);
}


exports._handleParseFailure = function _handleParseFailure(err, canceler, errback) {
  process.nextTick(function() {
    errback(err)();
  });
  var client = require('mongodb').MongoClient;
  return canceler(client);
};

exports._close = function _close(client, canceler, callback, left, right) {
  client.close(function(err, x) {
    (err ? callback(left(err)) : callback(right(x)))();
  });
  return canceler({});
};

exports._collection = function _collection(name, db, canceler, callback, left, right) {
  db.collection(name, function(err, x) {
    (err ? callback(left(err)) : callback(right(x)))();
  });
  return canceler(db);
};

exports._collect = function _collect(cursor, canceler, callback, left, right) {
  cursor.toArray(function(err, x) {
    (err ? callback(left(err)) : callback(right(x)))();
  });
  return canceler(cursor);
};

exports._collectOne = function _collectOne(cursor, canceler,  callback, left, right) {
  cursor.next(function(err, x) {
    if (err) {
      callback(left(err))();
    } else if (x === null) {
      var error = new Error('Not Found.');
      error.name = 'MongoError';
      callback(left(error))();
    } else {
      callback(right(x))();
    }
  });
  return canceler(cursor);
};

exports._findOne = function _findOne(selector, fields, collection, canceler, callback, left, right) {
  collection.findOne(selector, fields, function(err, x) {
    (err ? callback(left(err)) : callback(right(x)))();
  });
  return canceler(collection);
};

exports._find = function _find(selector, fields, collection, canceler, callback, left, right) {
  collection.find(selector, fields, function(err, x) {
    (err ? callback(left(err)) : callback(right(x)))();
  });
  return canceler(collection);
};

exports._insert = function _insert(type, json, options, collection, canceler, callback, left, right) {
  collection["insert" + type](json, options, function(err, x) {
    (err ? callback(left(err)) : callback(right(x.result)))();
  });
  return canceler(collection);
};

exports._update = function(type, selector, json, options, collection, canceler, callback, left, right) {
  collection["update" + type](selector, { $set: json }, options, function(err, x) {
    (err ? callback(left(err)) : callback(right(x.result)))();
  });

  return canceler(collection);
};
