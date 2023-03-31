'use strict';

var uuid = null;
exports._seathUUIDShow = uuidStr => {
  return uuidStr.slice(6,-1)
  }

// todo: we can use the validate function and the version function instead 
// of parse
exports._seathParseUUID = just => nothing => str =>  {
  if (uuid === null) uuid = require('uuid');
  var slicedString =str.slice(6,-1)
  try {uuid.parse(slicedString); return just(slicedString)} catch (e) {
    try {uuid.parse(str); return just(str)} catch (e){
    return nothing
    };
  };
};

