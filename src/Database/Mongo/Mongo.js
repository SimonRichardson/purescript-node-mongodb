// module Database.Mongo.Mongo

'use strict';

exports._connect = function _connect(uri, canceler, errback, callback) {
  var client = require('mongodb').MongoClient;
  client.connect(uri, function(err, x) {
    (err ? errback(err) : callback(x))();
  });
  return canceler(client);
};

exports._handleParseFailure = function _handleParseFailure(err, canceler, errback) {
  process.nextTick(function() {
    errback(err)();
  });
  var client = require('mongodb').MongoClient;
  return canceler(client);
};

exports._close = function _close(db, canceler, errback, callback) {
  db.close(function(err, x) {
    (err ? errback(err) : callback(x))();
  });
  return canceler({});
};

exports._collection = function _collection(name, db, canceler, errback, callback) {
  db.collection(name, function(err, x) {
    (err ? errback(err) : callback(x))();
  });
  return canceler(db);
};

exports._collect = function _collect(cursor, canceler, errback, callback) {
  cursor.toArray(function(err, x) {
    (err ? errback(err) : callback(x))();
  });
  return canceler(cursor);
};

exports._collectOne = function _collectOne(cursor, canceler, errback, callback) {
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
};

exports._findOne = function _findOne(selector, fields, collection, canceler, errback, callback) {
  collection.findOne(selector, fields, function(err, x) {
    (err ? errback(err) : callback(x))();
  });
  return canceler(collection);
};

exports._find = function _find(selector, fields, collection, canceler, errback, callback) {
  collection.find(selector, fields, function(err, x) {
    (err ? errback(err) : callback(x))();
  });
  return canceler(collection);
};

exports._insert = function _insert(type, json, options, collection, canceler, errback, callback) {
  collection["insert" + type](json, options, function(err, x) {
    (err ? errback(err) : callback(x.result))();
  });
  return canceler(collection);
};

exports._update = function _update(type, selector, json, options, collection, canceler, errback, callback) {
  collection["update" + type](selector, json, options, function(err, x) {
    (err ? errback(err) : callback(x.result))();
  });
  return canceler(collection);
};
