{-# LANGUAGE OverloadedStrings #-}

-- HOW TO RUN IT:
--
-- stack runghc GrabSounds.hs

import Data.Binary.Get
import Control.Monad.Trans.Class
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Monoid ((<>))
import Data.Char (ord)
import System.IO (Handle, IOMode(..), SeekMode(..), withBinaryFile, hSeek)

assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error msg

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
      putStrLn ("writing sound data to " <> filename <> " bytes " <> show (B.length bytes))
      B.writeFile "sounds.bin" bytes

readC64RAM :: Handle -> IO ()
readC64RAM h = B.hGet h  (4+0x10000+15) >>= (\bs -> dumpSounds (runGet parseRAM bs))
  where
    parseRAM = do
      skip 4
      ram <- getLazyByteString 0x10000
      skip 15
      return ram

readModules :: Handle -> IO ()
readModules h = do
  hdr <- B.hGet h (16+2+4)
  if hdr == B.empty then
    return ()
  else do
    let (name, vmajor, vminor, size) = runGet parseModuleHeader hdr
    B.putStrLn ("module: " <> name)
    putStrLn ("  module version: " <> show vmajor <> "." <> show vminor)
    putStrLn ("  content size: " <> show size)
    readModule (B.takeWhile (\c -> c /= '\0') name) size
    readModules h
  where
    parseModuleHeader = do
      mname  <- getLazyByteString 16
      vmajor <- getWord8
      vminor <- getWord8
      size   <- getWord32le
      return (mname, vmajor, vminor, fromIntegral size - 22)

    readModule "C64MEM" _ = do
      readC64RAM h
    readModule _modname size = do
      hSeek h RelativeSeek size

readMagic :: Handle -> IO ()
readMagic h = do
  -- The magic format doesn't actually match what's written in VICE docs. :(
  -- Figured this out by looking at the file in hex editor and then reading
  -- VICE source code
  -- (https://gist.github.com/nurpax/71aa57be0b819163bbf950715b41e3e0).
  magicHdr <- B.hGet h (19+1+1+16+13+8)
  let (magic, vmajor, vminor, machine) = runGet parseHeader magicHdr
  assert (B.isPrefixOf "VICE Snapshot File" magic) "invalid magic"
  assert (B.isPrefixOf "C64" machine) "only c64 is supported"
  putStrLn ("File format version: " <> show vmajor <> "." <> show vminor)
  readModules h
  where
    parseHeader = do
      magic  <- getLazyByteString 19
      vmajor <- getWord8
      vminor <- getWord8
      machineName <- getLazyByteString 16
      return (magic, vmajor, vminor, machineName)

main :: IO ()
main = withBinaryFile "snapshot.vsf" ReadMode readMagic
