{-# language OverloadedStrings #-}
module Test where

import qualified Postmark.Request as PReq
import qualified Postmark.Response as PRes
import Data.Aeson
import Data.Aeson.Text
import qualified Data.Text.Lazy.IO as TIO
import Postmark
import qualified System.IO.Streams as Streams

testStatus :: Either String PRes.Status
testStatus = eitherDecode "{ \"ErrorCode\":405, \"Message\":\"LOL\" }"

testE =
  PReq.Email {
    PReq.from = "sender@example.com"
  , PReq.to = ("receiver@example.com",["addrec@example.com"])
  , PReq.cc = ["copied@example.com"]
  , PReq.bcc = ["blank-copied@example.com"]
  , PReq.subject = Just "Test"
  , PReq.tag = Just "Invitation"
  , PReq.body = PReq.HtmlPlain "<b>Hello</b>" "Hello"
  , PReq.replyTo = Just "reply@example.com"
  , PReq.headers = [("CUSTOM-HEADER","value")]
  , PReq.trackOpens = True
  , PReq.trackLinks = PReq.None
  , PReq.attachments = [PReq.Attachment {
                          PReq.name = "readme.txt"
                        , PReq.content = "dGVzdCBjb250ZW50"
                        , PReq.contentType = "text/plain" }
                       ,PReq.Attachment {
                          PReq.name = "report.pdf"
                       ,  PReq.content = "dGVzdCBjb250ZW50"
                       ,  PReq.contentType = "application/octet-stream" }] }

runTestE = TIO.putStrLn $ encodeToLazyText testE  

testResponse :: Either String PRes.Success
testResponse = eitherDecode "{ \"To\": \"receiver@example.com\", \"SubmittedAt\": \"2014-02-17T07:25:01.4178645-05:00\", \"MessageID\": \"0a129aee-e1cd-480d-b08d-4f48548ff48d\", \"ErrorCode\": 0, \"Message\": \"OK\" }"

myToken = "4095b392-b181-455d-be10-524ee376937e"

testSend = send "POSTMARK_API_TEST" testE

testRealE c =
  PReq.Email {
    PReq.from = "notify@boka-pass.se"
  , PReq.to = ("jon.petter.bergman@gmail.com",[])
  , PReq.cc = []
  , PReq.bcc = []
  , PReq.subject = Just "Test"
  , PReq.tag = Just "Invitation"
  , PReq.body = PReq.Plain c
  , PReq.replyTo = Nothing
  , PReq.headers = []
  , PReq.trackOpens = True
  , PReq.trackLinks = PReq.None
  , PReq.attachments = [PReq.Attachment {
                          PReq.name = "readme.txt"
                        , PReq.content = "dGVzdCBjb250ZW50"
                        , PReq.contentType = "text/plain" }] }


testSendReal = send myToken $ testRealE "kek"

testSendMany = sendStream myToken (\o -> Streams.writeList [testRealE "a",testRealE "c"] o >> Streams.write Nothing o)
                                  Streams.toList
