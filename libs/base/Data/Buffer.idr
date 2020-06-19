module Data.Buffer

import System.Directory
import System.File

export
data Buffer : Type where [external]

%foreign  "scheme:blodwen-buffer-size"
          "es2020:idris2_bufferSize,idris2_support"
prim__bufferSize : Buffer -> Int

export
rawSize : Buffer -> IO Int
rawSize buf = pure (prim__bufferSize buf)

%foreign "scheme:blodwen-new-buffer"
         "es2020:idris2_newBuffer,idris2_support"
prim__newBuffer : Int -> PrimIO Buffer

export
newBuffer : Int -> IO (Maybe Buffer)
newBuffer size
    = do buf <- primIO (prim__newBuffer size)
         pure $ Just buf
--          if prim__nullAnyPtr buf /= 0
--             then pure Nothing
--             else pure $ Just $ MkBuffer buf size 0

-- might be needed if we do this in C...
export
freeBuffer : Buffer -> IO ()
freeBuffer buf = pure ()

%foreign "scheme:blodwen-buffer-setbyte"
         "es2020:idris2_bufferSetByte,idris2_support"
prim__setByte : Buffer -> Int -> Int -> PrimIO ()
%foreign "scheme:blodwen-buffer-setbyte"
prim__setBits8 : Buffer -> Int -> Bits8 -> PrimIO ()

-- Assumes val is in the range 0-255
export
setByte : Buffer -> (loc : Int) -> (val : Int) -> IO ()
setByte buf loc val
    = primIO (prim__setByte buf loc val)

export
setBits8 : Buffer -> (loc : Int) -> (val : Bits8) -> IO ()
setBits8 buf loc val
    = primIO (prim__setBits8 buf loc val)

%foreign "scheme:blodwen-buffer-getbyte"
         "es2020:idris2_bufferGetByte,idris2_support"
prim__getByte : Buffer -> Int -> PrimIO Int
%foreign "scheme:blodwen-buffer-getbyte"
prim__getBits8 : Buffer -> Int -> PrimIO Bits8

export
getByte : Buffer -> (loc : Int) -> IO Int
getByte buf loc
    = primIO (prim__getByte buf loc)

export
getBits8 : Buffer -> (loc : Int) -> IO Bits8
getBits8 buf loc
    = primIO (prim__getBits8 buf loc)

%foreign "scheme:blodwen-buffer-setbits16"
prim__setBits16 : Buffer -> Int -> Bits16 -> PrimIO ()

export
setBits16 : Buffer -> (loc : Int) -> (val : Bits16) -> IO ()
setBits16 buf loc val
    = primIO (prim__setBits16 buf loc val)

%foreign "scheme:blodwen-buffer-getbits16"
prim__getBits16 : Buffer -> Int -> PrimIO Bits16

export
getBits16 : Buffer -> (loc : Int) -> IO Bits16
getBits16 buf loc
    = primIO (prim__getBits16 buf loc)

%foreign "scheme:blodwen-buffer-setbits32"
prim__setBits32 : Buffer -> Int -> Bits32 -> PrimIO ()

export
setBits32 : Buffer -> (loc : Int) -> (val : Bits32) -> IO ()
setBits32 buf loc val
    = primIO (prim__setBits32 buf loc val)

%foreign "scheme:blodwen-buffer-getbits32"
prim__getBits32 : Buffer -> Int -> PrimIO Bits32

export
getBits32 : Buffer -> (loc : Int) -> IO Bits32
getBits32 buf loc
    = primIO (prim__getBits32 buf loc)

%foreign "scheme:blodwen-buffer-setbits64"
prim__setBits64 : Buffer -> Int -> Bits64 -> PrimIO ()

export
setBits64 : Buffer -> (loc : Int) -> (val : Bits64) -> IO ()
setBits64 buf loc val
    = primIO (prim__setBits64 buf loc val)

%foreign "scheme:blodwen-buffer-getbits64"
prim__getBits64 : Buffer -> Int -> PrimIO Bits64

export
getBits64 : Buffer -> (loc : Int) -> IO Bits64
getBits64 buf loc
    = primIO (prim__getBits64 buf loc)

%foreign "scheme:blodwen-buffer-setint32"
         "es2020:idris2_bufferSetInt32,idris2_support"
prim__setInt32 : Buffer -> Int -> Int -> PrimIO ()

export
setInt32 : Buffer -> (loc : Int) -> (val : Int) -> IO ()
setInt32 buf loc val
    = primIO (prim__setInt32 buf loc val)

%foreign "scheme:blodwen-buffer-getint32"
         "es2020:idris2_bufferGetInt32,idris2_support"
prim__getInt32 : Buffer -> Int -> PrimIO Int

export
getInt32 : Buffer -> (loc : Int) -> IO Int
getInt32 buf loc
    = primIO (prim__getInt32 buf loc)

%foreign "scheme:blodwen-buffer-setint"
         "es2020:idris2_bufferSetInt,idris2_support"
prim__setInt : Buffer -> Int -> Int -> PrimIO ()

export
setInt : Buffer -> (loc : Int) -> (val : Int) -> IO ()
setInt buf loc val
    = primIO (prim__setInt buf loc val)

%foreign "scheme:blodwen-buffer-getint"
         "es2020:idris2_bufferGetInt,idris2_support"
prim__getInt : Buffer -> Int -> PrimIO Int

export
getInt : Buffer -> (loc : Int) -> IO Int
getInt buf loc
    = primIO (prim__getInt buf loc)

%foreign "scheme:blodwen-buffer-setdouble"
         "es2020:idris2_bufferSetDouble,idris2_support"
prim__setDouble : Buffer -> Int -> Double -> PrimIO ()

export
setDouble : Buffer -> (loc : Int) -> (val : Double) -> IO ()
setDouble buf loc val
    = primIO (prim__setDouble buf loc val)

%foreign "scheme:blodwen-buffer-getdouble"
         "es2020:idris2_bufferGetDouble,idris2_support"
prim__getDouble : Buffer -> Int -> PrimIO Double

export
getDouble : Buffer -> (loc : Int) -> IO Double
getDouble buf loc
    = primIO (prim__getDouble buf loc)

-- Get the length of a string in bytes, rather than characters
export
%foreign "C:strlen,libc 6"
         "es2020:idris2_stringLength,idris2_support"
stringByteLength : String -> Int

%foreign "scheme:blodwen-buffer-setstring"
         "es2020:idris2_bufferSetString,idris2_support"
prim__setString : Buffer -> Int -> String -> PrimIO ()

export
setString : Buffer -> (loc : Int) -> (val : String) -> IO ()
setString buf loc val
    = primIO (prim__setString buf loc val)

%foreign "scheme:blodwen-buffer-getstring"
         "es2020:idris2_bufferGetString,idris2_support"
prim__getString : Buffer -> Int -> Int -> PrimIO String

export
getString : Buffer -> (loc : Int) -> (len : Int) -> IO String
getString buf loc len
    = primIO (prim__getString buf loc len)

export
bufferData : Buffer -> IO (List Int)
bufferData buf
    = do len <- rawSize buf
         unpackTo [] len
  where
    unpackTo : List Int -> Int -> IO (List Int)
    unpackTo acc 0 = pure acc
    unpackTo acc loc
        = do val <- getByte buf (loc - 1)
             unpackTo (val :: acc) (loc - 1)

%foreign "scheme:blodwen-buffer-copydata"
         "es2020:idris2_bufferCopyData,idris2_support"
prim__copyData : Buffer -> Int -> Int -> Buffer -> Int -> PrimIO ()

export
copyData : (src : Buffer) -> (start, len : Int) ->
           (dest : Buffer) -> (loc : Int) -> IO ()
copyData src start len dest loc
    = primIO (prim__copyData src start len dest loc)

-- %foreign "scheme:blodwen-readbuffer-bytes"
-- prim__readBufferBytes : FilePtr -> AnyPtr -> Int -> Int -> PrimIO Int
--
-- export
-- readBufferFromFile : BinaryFile -> Buffer -> (maxbytes : Int) ->
--                      IO (Either FileError Buffer)
-- readBufferFromFile (FHandle h) (MkBuffer buf size loc) max
--     = do read <- primIO (prim__readBufferBytes h buf loc max)
--          if read >= 0
--             then pure (Right (MkBuffer buf size (loc + read)))
--             else pure (Left FileReadError)

%foreign "scheme:blodwen-read-bytevec"
         "es2020:idris2_bufferFromFile,idris2_support"
prim__readBufferFromFile : String -> String -> PrimIO Buffer

%foreign "scheme:blodwen-isbytevec"
         "es2020:idris2_isBuffer,idris2_support"
prim__isBuffer : Buffer -> Int

-- Create a new buffer by reading all the contents from the given file
-- Fails if no bytes can be read or buffer can't be created
export
createBufferFromFile : String -> IO (Either FileError Buffer)
createBufferFromFile fn
    = do Just cwd <- currentDir
              | Nothing => pure (Left FileReadError)
         buf <- primIO (prim__readBufferFromFile cwd fn)
         if prim__isBuffer buf /= 0
            then pure (Left FileReadError)
            else do let sz = prim__bufferSize buf
                    pure (Right buf)

%foreign "scheme:blodwen-write-bytevec"
         "es2020:idris2_writeBufferToFile,idris2_support"
prim__writeBuffer : String -> String -> Buffer -> Int -> PrimIO Int

export
writeBufferToFile : String -> Buffer -> (maxbytes : Int) ->
                    IO (Either FileError ())
writeBufferToFile fn buf max
    = do Just cwd <- currentDir
              | Nothing => pure (Left FileReadError)
         res <- primIO (prim__writeBuffer cwd fn buf max)
         if res /= 0
            then pure (Left FileWriteError)
            else pure (Right ())

export
resizeBuffer : Buffer -> Int -> IO (Maybe Buffer)
resizeBuffer old newsize
    = do Just buf <- newBuffer newsize
              | Nothing => pure Nothing
         -- If the new buffer is smaller than the old one, just copy what
         -- fits
         oldsize <- rawSize old
         let len = if newsize < oldsize then newsize else oldsize
         copyData old 0 len buf 0
         freeBuffer old
         pure (Just buf)
