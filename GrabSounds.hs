{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Data.Char (ord)
import System.IO (Handle, IOMode(..), SeekMode(..), withBinaryFile, hSetBinaryMode, hSeek)

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
main = do
  withBinaryFile "snapshot.vsf" ReadMode $ \h -> do
    readMagic h
