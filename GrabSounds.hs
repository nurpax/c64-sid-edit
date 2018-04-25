{-# LANGUAGE OverloadedStrings #-}

import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Monoid ((<>))
import Data.Char (ord)
import System.IO (Handle, IOMode(..), SeekMode(..), withBinaryFile, hSeek)

assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error msg

bytesToInt :: ByteString -> Integer
bytesToInt bs = fromIntegral (b0 + b1*256 + b2*65536 + b3*65536*256)
  where
    b0 = ord . B.index bs $ 0
    b1 = ord . B.index bs $ 1
    b2 = ord . B.index bs $ 2
    b3 = ord . B.index bs $ 3

showByte :: ByteString -> ByteString
showByte = B.pack . show . ord . B.head

dumpSounds :: ByteString -> IO ()
dumpSounds ram = do
  save (runGet getSounds ram)
  where
    getSounds = do
      skip (0x3000-2)
      paramLen <- getWord16le
      getLazyByteString (fromIntegral paramLen)
    save bytes = do
      let filename = "sounds.bin"
      putStrLn ("writing sound data to " <> filename)
      B.writeFile "sounds.bin" bytes

readC64RAM :: Handle -> IO ()
readC64RAM h = do
  B.putStrLn "Reading C64MEM"
  hSeek h RelativeSeek 4 -- skip header bytes
  ramBytes <- B.hGet h 0x10000
  dumpSounds ramBytes
  hSeek h RelativeSeek 15 -- skip footer bytes

readModules :: Handle -> IO ()
readModules h = do
  moduleName <- B.hGet h 16
  if moduleName == B.empty then return ()
  else do
    B.putStrLn ("module: " <> moduleName)
    vmajor <- B.hGet h 1
    vminor <- B.hGet h 1
    B.putStrLn ("Module version: " <> showByte vmajor <> "." <> showByte vminor)
    size <- ((+) (-22) . bytesToInt) <$> B.hGet h 4
    putStrLn ("content size: " <> show size)
    readModule (B.takeWhile (\c -> c /= '\0') moduleName) size
  where
    readModule "C64MEM" _ = do
      readC64RAM h
      readModules h
    readModule _modname size = do
      hSeek h RelativeSeek size
      readModules h

readMagic :: Handle -> IO ()
readMagic h = do
  magic <- B.hGet h 19
  B.putStrLn magic
  assert (B.isPrefixOf "VICE Snapshot File" magic) "invalid magic"
  vmajor <- B.hGet h 1
  vminor <- B.hGet h 1
  B.putStrLn ("File format version: " <> showByte vmajor <> "." <> showByte vminor)
  _machineName <- B.hGet h 16
  -- This part is undocumented in VICE docs. :( Figured this out by looking at
  -- the file in hex editor and then reading VICE source code
  -- (https://gist.github.com/nurpax/71aa57be0b819163bbf950715b41e3e0).
  versionMagic <- B.hGet h 13
  B.putStrLn magic
  _viceVersion <- B.hGet h 8
  readModules h
  return ()

main :: IO ()
main = withBinaryFile "snapshot.vsf" ReadMode readMagic
