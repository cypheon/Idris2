function js2idris(x) {
  if (typeof x === 'undefined') {
    return undefined;
  }
  if (typeof x === 'number') {
    return BigInt(x);
  }
  if (typeof x === 'bigint') {
    return x;
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
  if (a.length === 0) {
    return [0];
  } else {
    return [1, js2idris(a[0]), js2idris_array(a.slice(1))];
  }
}

const fs = require('fs');

// IO
exports.idris2_putStr = (str, _world) => {
  process.stdout.write(str);
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
  try {
    fs.mkdirSync(dir);
    __errno = null;
    return js2idris(0);
  } catch (e) {
    __errno = e;
    return js2idris(1);
  }
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

function FilePtr(fd, name) {
  this.fd = fd;
  this.buffer = Buffer.alloc(0);
  this.name = name;
  this.eof = false;
  return this;
}

FilePtr.prototype.readLine = function() {
  const LF = 0x0a;
  const readBuf = Buffer.alloc(1);
  let lineEnd = this.buffer.indexOf(LF);
  while (lineEnd === -1) {
    const bytesRead = fs.readSync(this.fd, readBuf);
    if (bytesRead === 0) {
      this.eof = true;
      break;
    }
    this.buffer = Buffer.concat([this.buffer, readBuf.slice(0, bytesRead)]);
    lineEnd = this.buffer.indexOf(LF);
  }
  const line = this.buffer.slice(0, lineEnd + 1);
  this.buffer = this.buffer.slice(lineEnd + 1);
  return line.toString('utf-8');
}

const stdinFilePtr = new FilePtr(0, "<stdin>");
const stdoutFilePtr = new FilePtr(1, "<stdout>");
const stderrFilePtr = new FilePtr(2, "<stderr>");

// System.File
exports.idris2_stdin = (_world) => {
  return stdinFilePtr;
};
exports.idris2_stdout = (_world) => {
  return stdoutFilePtr;
};
exports.idris2_stderr = (_world) => {
  return stderrFilePtr;
};
exports.idris2_fileModifiedTime = (filePtr, _world) => {
  const stat = fs.fstatSync(filePtr.fd, {bigint: true});
  return stat.mtimeMs / BigInt(1000);
};

exports.idris2_readLine = (filePtr, _world) => {
  return filePtr.readLine();
};
exports.idris2_getStr = (_world) => {
  return stdinFilePtr.readLine();
};

exports.idris2_writeLine = (filePtr, line, _world) => {
  fs.writeSync(filePtr.fd, line, undefined, 'utf-8');
};

exports.idris2_openFile = (name, mode, _world) => {
  try {
    const fd = fs.openSync(name, mode);
    __errno = null;
    return new FilePtr(fd, name);
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
exports.idris2_eof = (filePtr, _world) => {
  if (filePtr.eof === true) {
    return js2idris(1);
  } else {
    return js2idris(0);
  }
};


exports.idris2_chmod = (filename, mode, _world) => {
  try {
    fs.chmodSync(filename, Number(mode));
    __errno = null;
    return js2idris(0);
  } catch (e) {
    __errno = e;
    return js2idris(1);
  }
};

const child_process = require('child_process');

exports.idris2_system = (cmdline, _world) => {
  try {
    const proc = child_process.spawnSync(cmdline, undefined, {shell: true});
    return js2idris(proc.status);
  } catch (e) {
    return js2idris(1);
  }
};
exports.idris2_exit = (code, _world) => {
  process.exit(Number(code));
};

// Data.Buffer

const bufferFuncsLE = {
  readInt64: (buf, offset) => buf.readBigInt64LE(offset),
  readInt32: (buf, offset) => buf.readInt32LE(offset),
  readDouble: (buf, offset) => buf.readDoubleLE(offset),

  writeInt64: (buf, offset, val) => buf.writeBigInt64LE(val, offset),
  writeInt32: (buf, offset, val) => buf.writeInt32LE(val, offset),
  writeDouble: (buf, offset, val) => buf.writeDoubleLE(val, offset),
};
const bufferFuncs = bufferFuncsLE;

exports.idris2_bufferFromFile = (dir, filename, _world) => {
  const path = filename.startsWith('/') ? filename : (dir + '/' + filename);
  try {
    return fs.readFileSync(path);
  } catch (e) {
  }
  return undefined;
};
exports.idris2_writeBufferToFile = (dir, filename, buf, maxBytes, _world) => {
  const path = filename.startsWith('/') ? filename : (dir + '/' + filename);
  try {
    fs.writeFileSync(path, buf.slice(0, Number(maxBytes)));
    return js2idris(0);
  } catch (e) {
    return js2idris(1);
  }
};
exports.idris2_isBuffer = (buf) => {
  if (buf !== undefined) {
    return js2idris(0);
  } else {
    return js2idris(1);
  }
};
exports.idris2_bufferSize = (buf) => {
  return js2idris(buf.length);
};

exports.idris2_bufferGetInt = (buf, offset, _world) => {
  return js2idris(bufferFuncs.readInt64(buf, Number(offset)));
};
exports.idris2_bufferGetInt32 = (buf, offset, _world) => {
  return js2idris(bufferFuncs.readInt32(buf, Number(offset)));
};
exports.idris2_bufferGetDouble = (buf, offset, _world) => {
  // careful: do not convert double to BigInt!
  return bufferFuncs.readDouble(buf, Number(offset));
};
exports.idris2_bufferGetByte = (buf, offset, _world) => {
  return js2idris(buf.readUInt8(Number(offset)));
};
exports.idris2_bufferGetString = (buf, offset, length, _world) => {
  return buf.slice(Number(offset), Number(offset + length)).toString('utf-8');
};

exports.idris2_bufferSetInt = (buf, offset, value, _world) => {
  bufferFuncs.writeInt64(buf, Number(offset), value);
};
exports.idris2_bufferSetInt32 = (buf, offset, value, _world) => {
  bufferFuncs.writeInt32(buf, Number(offset), Number(value));
};
exports.idris2_bufferSetDouble = (buf, offset, value, _world) => {
  bufferFuncs.writeDouble(buf, Number(offset), value);
};
exports.idris2_bufferSetByte = (buf, offset, value, _world) => {
  buf.writeUInt8(Number(value), Number(offset));
};
exports.idris2_bufferSetString = (buf, offset, value, _world) => {
  buf.write(value, Number(offset), buf.length - Number(offset), 'utf-8');
};

exports.idris2_stringLength = (str) => {
  return js2idris(Buffer.from(str, 'utf-8').length);
};

exports.idris2_bufferCopyData = (srcBuf, srcOffset, length, destBuf, destOffset, _world) => {
  srcBuf.copy(destBuf, Number(destOffset), Number(srcOffset), Number(srcOffset + length));
  return undefined;
};

exports.idris2_newBuffer = (size, _world) => {
  return Buffer.alloc(Number(size));
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

const id = (x) => x;

function idris2js_List(x) {
  const result = [];
  let cursor = x;
  while (Number(cursor[0]) !== 0) {
    result.push(cursor[2]);
    cursor = cursor[3];
  }
  return result;
}

function __schemeCall__string_append(value) {
  const parts = idris2js_List(value);
  return "".concat(...parts);
}
exports.idris2_schemeCall = function (returnTypes, functionName) {
  const arrayArguments = [].slice.call(arguments);
  const args = arrayArguments.slice(2);
  if (functionName === 'string-append') {
    return __schemeCall__string_append(args[0]);
  }
  throw new Error("schemeCall not implemented: " + JSON.stringify(arguments));
};