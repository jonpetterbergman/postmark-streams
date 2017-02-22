{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: $HEADER$
--
-- Postmark email requests.
module Postmark.Request(Email(..),TrackLinks(..),Attachment(..),Body(..)) where

import           Data.Aeson             (ToJSON (..), Value (..), object, (.=))
import           Data.Aeson.Types       (Pair)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Text              (Text, intercalate)
import           Data.Text.Encoding     (decodeUtf8)

-- | Body can be html, plain-text or both
data Body =
    Html Text
  | Plain Text
  | HtmlPlain Text Text

encBody :: Body -> [Pair]
encBody (Html v)        = ["HtmlBody" .= v]
encBody (Plain p)       = ["TextBody" .= p]
encBody (HtmlPlain v p) = ["HtmlBody" .= v,"TextBody" .= p]

encHeaders :: [(Text,Text)] -> Value
encHeaders = toJSON . map go
  where go (k,v) = object ["Name" .= k,"Value" .= v]

-- | Activate link-tracking for the selected body types.
data TrackLinks =
    None
  | HtmlAndText
  | HtmlOnly
  | TextOnly

encTrackLinks :: TrackLinks -> Text
encTrackLinks None        = "None"
encTrackLinks HtmlAndText = "HtmlAndText"
encTrackLinks HtmlOnly    = "HtmlOnly"
encTrackLinks TextOnly    = "TextOnly"

-- | The type of attachments.
data Attachment =
  Attachment {
    name        :: Text
  , content     :: ByteString
  , contentType :: Text }

instance ToJSON Attachment where
  toJSON a = object
    ["Name" .= name a,
     "Content" .= (decodeUtf8 $ Base64.encode $ content a),
     "ContentType" .= contentType a]

-- | The type of email requests.
data Email =
  Email {
    from        :: Text
  , to          :: (Text,[Text])
  , cc          :: [Text]
  , bcc         :: [Text]
  , subject     :: Maybe Text
  , tag         :: Maybe Text
  , body        :: Body
  , replyTo     :: Maybe Text
  , headers     :: [(Text,Text)]
  , trackOpens  :: Bool
  , trackLinks  :: TrackLinks
  , attachments :: [Attachment] }

toCommaSeparated :: [Text] -> Text
toCommaSeparated = intercalate ","

instance ToJSON Email where
  toJSON r = object $ concat
    [["From" .= from r
     ,"To" .= (toCommaSeparated $ (\(p,xs) -> p:xs) $ to r)
     ,"Cc" .= (toCommaSeparated $ cc r)
     ,"Bcc" .= (toCommaSeparated $ bcc r)]
    ,maybe [] (\s -> ["Subject" .= s]) $ subject r
    ,maybe [] (\t -> ["Tag" .= t]) $ tag r
    ,encBody $ body r
    ,maybe [] (\rt -> ["ReplyTo" .= rt]) $ replyTo r
    ,["Headers" .= (encHeaders $ headers r)
     ,"TrackOpens" .= trackOpens r
     ,"TrackLinks" .= (encTrackLinks $ trackLinks r)
     ,"Attachments" .= attachments r]]
