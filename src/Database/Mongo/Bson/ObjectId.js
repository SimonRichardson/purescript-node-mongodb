// module Database.Mongo.Bson.ObjectId

'use strict';

exports.hexStringFromObject = function(objectId) {
  return objectId.toHexString()
};
