module Monadoc.Convert where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Time as Time
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql

stringToText :: String -> Text.Text
stringToText = Text.pack

textToUtf8 :: Text.Text -> ByteString.ByteString
textToUtf8 = Text.encodeUtf8

utf8ToText :: ByteString.ByteString -> Text.Text
utf8ToText = Text.decodeUtf8With Text.lenientDecode

stringToUtf8 :: String -> ByteString.ByteString
stringToUtf8 = textToUtf8 . stringToText

timeToString :: Time.UTCTime -> String
timeToString = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"

stringToQuery :: String -> Sql.Query
stringToQuery = Sql.Query . stringToText

versionToString :: Version.Version -> String
versionToString = Version.showVersion
