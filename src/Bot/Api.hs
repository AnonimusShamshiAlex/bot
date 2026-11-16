module Bot.Api
  ( Client
  , Update (..)
  , Message (..)
  , newClient
  , poll
  , say
  , sayWith
  , sendDocument
  , configure
  ) where

import Control.Monad (void)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData (formDataBody, partBS, partFileRequestBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

data Client = Client
  { manager :: Manager
  , base :: String
  }

data Update = Update
  { updateId :: Int
  , incoming :: Maybe Message
  }

data Message = Message
  { chatId :: Int
  , userId :: Int
  , body :: Maybe Text
  }

newtype Envelope a = Envelope a

instance FromJSON a => FromJSON (Envelope a) where
  parseJSON = withObject "Envelope" $ \o -> do
    ok <- o .: "ok"
    if ok then Envelope <$> o .: "result" else fail "telegram returned ok=false"

instance FromJSON Update where
  parseJSON = withObject "Update" $ \o ->
    Update <$> o .: "update_id" <*> o .:? "message"

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    chat <- o .: "chat"
    from <- o .: "from"
    Message <$> chat .: "id" <*> from .: "id" <*> o .:? "text"

newClient :: String -> IO Client
newClient token = do
  m <- newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 120000000 }
  pure (Client m ("https://api.telegram.org/bot" <> token <> "/"))

call :: Client -> String -> Value -> IO (Response BL.ByteString)
call c name payload = do
  req <- parseRequest (base c <> name)
  httpLbs
    req
    { method = "POST"
    , requestHeaders = [("Content-Type", "application/json")]
    , requestBody = RequestBodyLBS (encode payload)
    }
    (manager c)

poll :: Client -> Maybe Int -> IO [Update]
poll c off = do
  resp <- call c "getUpdates" . object $
    maybe [] (\o -> ["offset" .= o]) off
      <> [ "timeout" .= (30 :: Int)
         , "allowed_updates" .= (["message"] :: [Text])
         ]
  case eitherDecode (responseBody resp) of
    Left e -> fail e
    Right (Envelope us) -> pure us

sayWith :: Client -> Int -> Text -> [Text] -> IO ()
sayWith c cid t buttons = void . call c "sendMessage" . object $
  [ "chat_id" .= cid, "text" .= t ]
    <> [ "reply_markup" .= keyboard | not (null buttons) ]
  where
    keyboard = object
      [ "keyboard" .= [map (\b -> object ["text" .= b]) buttons]
      , "resize_keyboard" .= True
      , "one_time_keyboard" .= True
      ]

say :: Client -> Int -> Text -> IO ()
say c cid t = sayWith c cid t []

sendDocument :: Client -> Int -> FilePath -> BS.ByteString -> Text -> IO ()
sendDocument c cid name bytes caption = do
  req <- parseRequest (base c <> "sendDocument")
  withBody <- formDataBody
    [ partBS "chat_id" (TE.encodeUtf8 (T.pack (show cid)))
    , partBS "caption" (TE.encodeUtf8 caption)
    , partFileRequestBody "document" name (RequestBodyBS bytes)
    ]
    req
  void (httpLbs withBody (manager c))

configure :: Client -> [(Text, Text)] -> Text -> Text -> IO ()
configure c commands description shortDescription = do
  void . call c "setMyCommands" . object $
    [ "commands" .= [object ["command" .= n, "description" .= d] | (n, d) <- commands] ]
  void . call c "setMyDescription" . object $ [ "description" .= description ]
  void . call c "setMyShortDescription" . object $ [ "short_description" .= shortDescription ]
