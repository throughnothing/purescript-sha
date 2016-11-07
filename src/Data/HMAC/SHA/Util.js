// module Data.HMAC.SHA.Util
"use strict";

var jsSHA = require('jssha');

exports._shaHmacBuffer = function(alg) {
  return function(keyArr) {
    return function(dataArr) {
      var shaObj = new jsSHA(alg, "ARRAYBUFFER");
      shaObj.setHMACKey(keyArr, "ARRAYBUFFER");
      shaObj.update(dataArr);
      return new Uint8Array(shaObj.getHMAC("ARRAYBUFFER"));
    }
  }
}

exports._shaHmacHex = function(alg) {
  return function(keyArr) {
    return function(dataArr) {
      var shaObj = new jsSHA(alg, "ARRAYBUFFER");
      shaObj.setHMACKey(keyArr, "ARRAYBUFFER");
      shaObj.update(dataArr);
      return shaObj.getHMAC("HEX");
    }
  }
}

exports._shaHmacB64 = function(alg) {
  return function(keyArr) {
    return function(dataArr) {
      var shaObj = new jsSHA(alg, "ARRAYBUFFER");
      shaObj.setHMACKey(keyArr, "ARRAYBUFFER");
      shaObj.update(dataArr);
      return shaObj.getHMAC("B64");
    }
  }
}
