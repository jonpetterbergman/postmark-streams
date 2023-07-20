{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: $HEADER$
--
-- Send email via Postmark using io-streams.       
module Postmark(send,sendStream,Error(..)) where

import           Control.Applicative          ((<|>))
import           Control.Exception            (bracket)
import           Control.Monad                (when)
import           Data.Aeson                   (FromJSON, ToJSON, encode)
import           Data.Aeson                   (Result, Value, fromJSON, json')
import qualified Data.Aeson                   as Aeson
import           Data.Attoparsec.ByteString   (Parser)
import           Data.Binary.Builder          (Builder)
import qualified Data.Binary.Builder          as Builder
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as ByteString
import           Data.ByteString.Lazy         (toChunks)
import           Data.Char                    (isSpace)
import           Data.IORef                   (newIORef, readIORef, writeIORef)
import           Data.Maybe                   (isJust)
import           Debug.Trace
import           Network.Http.Client
import           Postmark.Request             (Email)
import qualified Postmark.Request             as PRes
import qualified Postmark.Response            as PRes
import           System.IO.Streams            (InputStream, OutputStream,
                                               makeInputStream,
                                               makeOutputStream, readExactly,
                                               takeBytesWhile, unRead, write)
import qualified System.IO.Streams            as Streams
import           System.IO.Streams.Attoparsec (parseFromStream)
import qualified System.IO.Streams.Attoparsec as AStreams
import           System.IO.Streams.Debug      (debugInputBS, debugOutput)

-- | Error type
data Error =
    Raw StatusCode ByteString 
  | API PRes.Error deriving Show

singleUrl :: ByteString
singleUrl = "https://api.postmarkapp.com/email"

batchUrl :: ByteString
batchUrl = "https://api.postmarkapp.com/email/batch"

-- | @'send' token email@ sends a single mail via Postmark.
--   @token@ is your Postmark server token or the test-token,
--   \"POSTMARK_API_TEST\" for testing purposes.
send :: ByteString -> Email -> IO (Either Error PRes.Success)
send token r = withConnection (establishConnection singleUrl) $ \connection ->
  do
    sendRequest connection (req singleUrl token) (jsonBodyP r)
    receiveResponse connection decodeResponse'

-- | @'sendStream' token build process@ sends a stream of emails
--   via Postmark's batch email API.
sendStream :: ByteString
           -> (OutputStream Email -> IO ())
           -> (InputStream (Either PRes.Error PRes.Success) -> IO r)
           -> IO (Either Error r)
sendStream token build process = withConnection (establishConnection batchUrl) $ \connection ->
  do
    sendRequest connection (req batchUrl token) $ \o -> build =<< jsonOutputStream o
    receiveResponse connection $ decodeResponse $ \i -> process =<< jsonInputStream successOrError i

req :: ByteString -> ByteString -> Request
req url token = buildRequest1 $ do
  http POST url
  setAccept "application/json"
  setContentType "application/json"
  setHeader "X-Postmark-Server-Token" token

jsonBodyP :: ToJSON a => a -> OutputStream Builder -> IO ()
jsonBodyP v o = write (Just $ Builder.fromLazyByteString $ encode v) o

decodeResponse :: (InputStream ByteString -> IO r)
               -> Response
               -> InputStream ByteString
               -> IO (Either Error r)
decodeResponse success r i = decodeStatus $ getStatusCode r
  where decodeStatus 200 = fmap Right $ success i
        decodeStatus 422 = fmap (Left . API) $ jsonFromStream fromJSON i
        decodeStatus x   = return $ Left $ Raw x $ getStatusMessage r

decodeResponse' :: Response -> InputStream ByteString -> IO (Either Error PRes.Success)
decodeResponse' = decodeResponse $ jsonFromStream fromJSON

jsonFromStream :: (Value -> Result a) -> InputStream ByteString -> IO a
jsonFromStream f i =
  do
    v <- parseFromStream json' i
    case f v of
      Aeson.Success x -> return x
      Aeson.Error str -> error str

delim :: a -> a -> a -> OutputStream a -> IO (OutputStream a)
delim start sep end o = newIORef True >>= makeOutputStream . f
  where f _ Nothing = write (Just end) o
        f sRef s = do atStart <- readIORef sRef
                      writeIORef sRef False
                      if atStart then write (Just start) o else write (Just sep) o
                      write s o

jsonOutputStream :: ToJSON a => OutputStream Builder -> IO (OutputStream a)
jsonOutputStream o =
  Streams.contramap (Builder.fromLazyByteString . encode) =<<
    delim "[\n" ",\n" "]\n\n" o

match :: ByteString -> InputStream ByteString -> IO (Maybe ByteString)
match m i = readExactly (ByteString.length m) i >>= go
  where go bs | bs == m = return $ Just bs
              | otherwise = do unRead bs i
                               return Nothing

space :: InputStream ByteString -> IO ()
space i = takeBytesWhile isSpace i >> return ()

spaceMatch :: ByteString -> InputStream ByteString -> IO (Maybe ByteString)
spaceMatch m i = space i >> match m i

spaceMatch' :: ByteString -> InputStream ByteString -> IO ()
spaceMatch' m i = fmap (maybe (error $ "match': expected: " ++ show m) (const ())) $ spaceMatch m i

jsonInputStream :: (Value -> Result a) -> InputStream ByteString -> IO (InputStream a)
jsonInputStream f i =
  do
    spaceMatch' "[" i
    as <- newIORef True
    makeInputStream $ go as
      where go as = spaceMatch "]" i >>= go'
                where go' end | isJust end = return Nothing
                              | otherwise =
                                do
                                  atStart <- readIORef as
                                  when (not atStart) $ spaceMatch' "," i
                                  writeIORef as False
                                  fmap Just $ jsonFromStream f i

successOrError :: Value -> Result (Either PRes.Error PRes.Success)
successOrError v = (fmap Left $ fromJSON v) <|> (fmap Right $ fromJSON v)
