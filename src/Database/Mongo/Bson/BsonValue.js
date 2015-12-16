// module Database.Mongo.Bson.BsonValue

'use strict';

function extract(v) {
  if(Array.isArray(v)) {
    return v.map(function(a) {
      return unsafeToBsonObject([a]);
    });
  }
  return v;
}

exports.unsafeToBsonObject = function unsafeToBsonObject(doc){
  return doc.reduce(function(b, a) {
    b[a["value0"]] = extract(a["value1"]["value0"]);
    return b;
  }, {});
};
