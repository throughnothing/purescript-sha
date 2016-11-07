// module Data.Digest.SHA.Util
"use strict";

var jsSHA = require('jssha');

exports._shaBuffer = function(alg) {
  return function(arr) {
    var shaObj = new jsSHA(alg, "ARRAYBUFFER");
    shaObj.update(arr);
    return new Uint8Array(shaObj.getHash("ARRAYBUFFER"));
  }
}

exports._shaHex = function(alg) {
  return function(arr) {
    var shaObj = new jsSHA(alg, "ARRAYBUFFER");
    shaObj.update(arr);
    return shaObj.getHash("HEX");
  }
}

exports._shaBase64 = function(alg) {
  return function(arr) {
    var shaObj = new jsSHA(alg, "ARRAYBUFFER");
    shaObj.update(arr);
    return shaObj.getHash("B64");
  }
}
