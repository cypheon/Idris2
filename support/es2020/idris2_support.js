function js2idris(x) {
  if (typeof x === 'undefined') {
    return undefined;
  }
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

const fs = require('fs');

// IO
exports.idris2_putStr = (str, _world) => {
  console.log(str);
};

// System
exports.idris2_getArgs = () => {
  return js2idris(process.argv.slice(1));
};
exports.idris2_getEnv = (name, _world) => {
  return js2idris(process.env[name]);
};

// mutable var to store last error
let __errno = null;
exports.idris2_fileErrno = (_world) => {
  if (__errno !== null) {
    const real_errno = __errno.errno;
    return js2idris(-real_errno);
  }
  return js2idris(0);
};

// System.Directory
exports.idris2_changeDir = (dir, _world) => {
  try {
    process.chdir(dir);
    __errno = null;
    return js2idris(0);
  } catch (e) {
    __errno = e;
    return js2idris(1);
  }
};
exports.idris2_currentDirectory = (_world) => {
  return js2idris(process.cwd());
};
exports.idris2_createDir = (dir, _world) => {
  throw new Error("not implemented");
};
exports.idris2_dirOpen = (dir, _world) => {
  throw new Error("not implemented");
};
exports.idris2_dirClose = (dir, _world) => {
  throw new Error("not implemented");
};
exports.idris2_rmDir = (dir, _world) => {
  throw new Error("not implemented");
};
exports.idris2_nextDirEntry = (dir, _world) => {
  throw new Error("not implemented");
};

function FilePtr(fd) {
  this.fd = fd;
  return this;
}

// System.File
exports.idris2_openFile = (name, mode, _world) => {
  try {
    const fd = fs.openSync(name, mode);
    __errno = null;
    return new FilePtr(fd);
  } catch (e) {
    __errno = e;
    return undefined;
  }
};

exports.idris2_closeFile = (filePtr, _world) => {
  try {
    const fd = filePtr.fd;
    fs.closeSync(fd);
    __errno = null;
  } catch (e) {
    __errno = e;
  }
  return undefined;
};

// AnyPtr
exports.idris2_isNull = (ptr) => {
  if (ptr === null || ptr === undefined) {
    return BigInt(1);
  }
  return BigInt(0);
};
exports.idris2_getString = (ptr) => {
  return ptr;
};
