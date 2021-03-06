{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: $HEADER$
--
-- Postmark email responses.
module Postmark.Response(Status(..),Error,Success(..)) where

import           Data.Aeson       (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types (typeMismatch)
import           Data.Text        (Text,split)
import           Data.Time        (ZonedTime)
import           Data.Word        (Word32)

-- | Status code
data Status =
  Status {
    errorCode :: Word32
  , message   :: Text
} deriving Show

-- | In case of an API error, only a 'Status' is returned
type Error = Status

instance FromJSON Status where
  parseJSON (Object o) = Status <$> o .: "ErrorCode" <*> o .: "Message"
  parseJSON invalid    = typeMismatch "Status" invalid

-- | In case of success, 'Success' is returned
data Success =
  Success {
    status      :: Status
  , submittedAt :: ZonedTime
  , messageID   :: Text
  , to_         :: [Text] } deriving Show

fromCommaSeparated :: Text -> [Text]
fromCommaSeparated = split (== ',')

instance FromJSON Success where
  parseJSON (Object o) =
    Success <$>
    parseJSON (Object o) <*>
    o .: "SubmittedAt" <*>
    o .: "MessageID" <*>
    fmap fromCommaSeparated (o .: "To")
  parseJSON invalid = typeMismatch "Success" invalid
