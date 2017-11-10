// module Database.Mongo.Bson.ObjectId

'use strict';

exports.hexStringFromObject = function(json) {
  return function() {
    return json.toHexString()
  };
};
