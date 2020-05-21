const __jsPrim_IdrisWorld = Symbol("__jsPrim_IdrisWorld");

const __jsPrim_integer_of_string = (s) => {
  const idx = s.indexOf(".");
  if (idx === -1) {
    return BigInt(s);
  } else {
    return BigInt(s.slice(0, idx));
  }
};

const __jsPrim_int_bound_63 = BigInt(2) ** BigInt(63);

function __jsPrim_open(path, mode, binary) {
  return BigInt(require("fs").openSync(path, mode));
}

function __jsPrim_close(fd) {
  return require("fs").closeSync(Number(fd));
}

function __jsPrim_idris_crash(msg) {
  throw new Error(msg);
}

function __jsPrim_putStr(s, __extra) {
  process.stdout.write(s);
}

function __jsPrim_newArray() {
  return new Array();
}

function __jsPrim_arrayGet(_erased, array, idx, _world) {
  return array[idx];
}

function __jsPrim_arraySet(_erased, array, idx, val, _world) {
  array[idx] = val;
}

function __jsPrim_IORef_impl(value) {
  this.value = value;
  return this;
}

function __jsPrim_newIORef(_erased, value, _world) {
  return new __jsPrim_IORef_impl(value);
}

function __jsPrim_readIORef(_erased, ref, _world) {
  return ref.value;
}

function __jsPrim_writeIORef(_erased, ref, value, _world) {
  ref.value = value;
}

function __jsPrim_reverseStr(s) {
  let r = "";
  for (let i = 1; i <= s.length; ++i) {
    r += s.charAt(s.length - i);
  }
  return r;
}
