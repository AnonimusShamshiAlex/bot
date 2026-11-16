module Bot.Ai (Ai, newAi, describe, generate, parseReply, rank) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.List (findIndex, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO (hPutStrLn, stderr)

import Bot.Types (Slide (..))

data Provider = Anthropic | Groq | OpenRouter
  deriving (Eq)

data Ai = Ai
  { manager :: Manager
  , key :: BS.ByteString
  , override :: Maybe Text
  , provider :: Provider
  }

newtype Block = Block Text

newtype Reply = Reply Text

newtype Chat = Chat Text

newtype Failure = Failure Text

newtype ModelList = ModelList [Text]

data Raw = Raw Text [Text]

instance FromJSON Block where
  parseJSON = withObject "Block" $ \o -> Block . fromMaybe "" <$> o .:? "text"

instance FromJSON Reply where
  parseJSON = withObject "Reply" $ \o -> do
    blocks <- o .: "content"
    pure (Reply (T.concat [t | Block t <- blocks]))

instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \o -> do
    choices <- o .: "choices"
    case choices of
      [] -> fail "no choices"
      (c : _) -> do
        m <- c .: "message"
        Chat . fromMaybe "" <$> m .:? "content"

instance FromJSON Failure where
  parseJSON = withObject "Failure" $ \o -> do
    e <- o .: "error"
    Failure <$> e .: "message"

instance FromJSON ModelList where
  parseJSON = withObject "ModelList" $ \o -> do
    items <- o .: "data"
    ModelList <$> traverse (.: "id") (items :: [Object])

instance FromJSON Raw where
  parseJSON = withObject "Raw" $ \o ->
    Raw <$> o .: "title" <*> (o .:? "bullets" .!= [])

baseUrl :: Provider -> String
baseUrl Anthropic = "https://api.anthropic.com/v1"
baseUrl Groq = "https://api.groq.com/openai/v1"
baseUrl OpenRouter = "https://openrouter.ai/api/v1"

providerName :: Provider -> Text
providerName Anthropic = "Anthropic"
providerName Groq = "Groq"
providerName OpenRouter = "OpenRouter"

detect :: String -> Maybe Provider
detect k
  | "sk-ant-" `isPrefixOf` k = Just Anthropic
  | "gsk_" `isPrefixOf` k = Just Groq
  | "sk-or-" `isPrefixOf` k = Just OpenRouter
  | otherwise = Nothing

newAi :: String -> Maybe Text -> IO (Maybe Ai)
newAi k over = case detect k of
  Nothing -> pure Nothing
  Just p -> do
    mgr <- newManager tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro 180000000 }
    pure (Just (Ai mgr (BC.pack k) over p))

describe :: Ai -> Text
describe ai = providerName (provider ai) <> maybe ", модель выбирается автоматически" (", модель " <>) (override ai)

usable :: Text -> Bool
usable m = not (any (`T.isInfixOf` T.toLower m)
  ["whisper", "guard", "tts", "orpheus", "embed", "compound", "allam", "moderation", "vision", "-vl", "coder"])

rank :: Provider -> [Text] -> [Text]
rank Groq ids = take 4 (known <> others)
  where
    preferred = ["openai/gpt-oss-120b", "qwen/qwen3.6-27b", "openai/gpt-oss-20b"]
    known = [m | m <- preferred, m `elem` ids]
    others = [m | m <- ids, m `notElem` preferred, usable m]
rank OpenRouter ids = take 4 (sortOn score free)
  where
    free = filter (\m -> ":free" `T.isSuffixOf` m && usable m) ids
    hints = ["gpt-oss-120b", "llama-3.3-70b", "qwen3", "gemma-3-27b", "deepseek", "mistral", "llama", "gemma"]
    score m = fromMaybe 99 (findIndex (`T.isInfixOf` m) hints)
rank Anthropic _ = ["claude-sonnet-5-5"]

fallbackModels :: Provider -> [Text]
fallbackModels Groq = ["openai/gpt-oss-120b", "qwen/qwen3.6-27b"]
fallbackModels OpenRouter = ["openai/gpt-oss-120b:free", "meta-llama/llama-3.3-70b-instruct:free"]
fallbackModels Anthropic = ["claude-sonnet-5-5"]

authHeaders :: Ai -> [(HeaderName, BS.ByteString)]
authHeaders ai = case provider ai of
  Anthropic -> [("x-api-key", key ai), ("anthropic-version", "2023-06-01")]
  _ -> [("authorization", "Bearer " <> key ai)]



liveModels :: Ai -> IO [Text]
liveModels ai = do
  r <- try $ do
    req0 <- parseRequest (baseUrl (provider ai) <> "/models")
    resp <- httpLbs req0 { requestHeaders = authHeaders ai, checkResponse = \_ _ -> pure () } (manager ai)
    pure (eitherDecode (responseBody resp))
  pure $ case r of
    Right (Right (ModelList ids)) -> ids
    Left e -> const [] (e :: SomeException)
    Right (Left _) -> []

candidates :: Ai -> IO [Text]
candidates ai = case override ai of
  Just m -> pure [m]
  Nothing -> do
    ids <- liveModels ai
    pure $ case rank (provider ai) ids of
      [] | null ids -> fallbackModels (provider ai)
      picked -> picked

systemPrompt :: Text
systemPrompt = T.unwords
  [ "You write the content of presentation slides."
  , "Reply with a JSON array and nothing else: no markdown fences, no commentary."
  , "Each element is an object with \"title\" (a string under 70 characters)"
  , "and \"bullets\" (an array of 3 to 5 short strings, each under 120 characters)."
  , "Write in the language of the topic."
  , "The first slide is an introduction and the last one is a summary."
  , "Be concrete and factual, and do not invent statistics or quotes."
  ]

attempt :: Ai -> Text -> Text -> Int -> IO (Either Text [Slide])
attempt ai model topic n = do
  req0 <- parseRequest (baseUrl (provider ai) <> path)
  let req = req0
        { method = "POST"
        , requestHeaders = authHeaders ai <> [("content-type", "application/json")]
        , requestBody = RequestBodyLBS (encode payload)
        , checkResponse = \_ _ -> pure ()
        }
  resp <- httpLbs req (manager ai)
  pure (parseReply (responseBody resp))
  where
    ask = "Topic: " <> topic <> "\nNumber of slides: " <> T.pack (show n)
    user = object ["role" .= ("user" :: Text), "content" .= ask]
    system = object ["role" .= ("system" :: Text), "content" .= systemPrompt]
    effort = [ "reasoning_effort" .= ("low" :: Text) | provider ai == Groq, "gpt-oss" `T.isInfixOf` model ]
    (path, payload) = case provider ai of
      Anthropic ->
        ( "/messages"
        , object
            [ "model" .= model
            , "max_tokens" .= (4096 :: Int)
            , "system" .= systemPrompt
            , "messages" .= [user]
            ]
        )
      _ ->
        ( "/chat/completions"
        , object $
            [ "model" .= model
            , "max_tokens" .= (8000 :: Int)
            , "temperature" .= (0.6 :: Double)
            , "messages" .= [system, user]
            ] <> effort
        )

fatal :: Text -> Bool
fatal e = any (`T.isInfixOf` T.toLower e) ["api key", "api_key", "auth", "credential", "unauthorized"]

generate :: Ai -> Text -> Int -> IO (Either Text [Slide])
generate ai topic n = do
  models <- candidates ai
  case models of
    [] -> pure (Left noModels)
    _ -> go models
  where
    noModels = case provider ai of
      OpenRouter -> "у этого ключа OpenRouter сейчас нет бесплатных моделей. Пополните баланс и задайте AI_MODEL, например meta-llama/llama-3.3-70b-instruct"
      _ -> "сервис не вернул подходящих моделей"
    go [] = pure (Left noModels)
    go (m : rest) = do
      r <- attempt ai m topic n
      case r of
        Right sl -> pure (Right sl)
        Left e
          | fatal e || null rest -> pure (Left e)
          | otherwise -> do
              hPutStrLn stderr ("модель " <> T.unpack m <> " не подошла: " <> T.unpack e)
              go rest

parseReply :: BL.ByteString -> Either Text [Slide]
parseReply body = case eitherDecode body of
  Right (Reply txt) -> slidesFrom txt
  Left _ -> case eitherDecode body of
    Right (Chat txt) -> slidesFrom txt
    Left _ -> case eitherDecode body of
      Right (Failure m) -> Left ("ошибка API: " <> m)
      Left _ -> Left "сервер вернул непонятный ответ"

slidesFrom :: Text -> Either Text [Slide]
slidesFrom txt = case eitherDecodeStrict (TE.encodeUtf8 arrayText) of
  Left _ -> Left "модель вернула слайды в неожиданном виде"
  Right raws -> case mapMaybe clean raws of
    [] -> Left "модель не вернула ни одного слайда"
    xs -> Right (take 15 xs)
  where
    arrayText = T.dropWhileEnd (/= ']') (T.dropWhile (/= '[') txt)

clean :: Raw -> Maybe Slide
clean (Raw t bullets)
  | T.null title = Nothing
  | otherwise = Just (Slide (T.take 100 title) (take 12 (filter (not . T.null) (map (T.take 250 . T.strip) bullets))))
  where
    title = T.strip t
