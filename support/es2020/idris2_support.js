function js2idris(x) {
  if (typeof x === 'number') {
    return BigInt(x);
  }
  if (typeof x === 'string') {
    return x;
  }
  if (x.length !== undefined && typeof x.slice === 'function') {
    return js2idris_array(x);
  }
  console.error("do not know how to convert object from JS to Idris:", x);
  return x;
}

function js2idris_array(a) {
  console.log("convert: ", a);
  if (a.length === 0) {
    return [0];
  } else {
    return [1, js2idris(a[0]), js2idris_array(a.slice(1))];
  }
}

exports.idris2_putStr = (str, _world) => {console.log(str);};
exports.idris2_getArgs = () => {
  return js2idris(process.argv.slice(1));
};
