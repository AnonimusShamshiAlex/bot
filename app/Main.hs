module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Bot.Ai
import Bot.Api
import Bot.Logic
import Bot.Pptx
import Bot.Store
import Bot.Types

data Env = Env
  { envClient :: Client
  , envStore :: Store
  , envAdmins :: Set Int
  , envAi :: Maybe Ai
  , envLimit :: Int
  }

templateDir :: FilePath
templateDir = "template"

parseAdmins :: String -> Set Int
parseAdmins = Set.fromList . mapMaybe readMaybe . words . map (\ch -> if ch == ',' then ' ' else ch)

firstEnv :: [String] -> IO (Maybe String)
firstEnv [] = pure Nothing
firstEnv (n : ns) = do
  v <- lookupEnv n
  case nonEmpty v of
    Just x -> pure (Just x)
    Nothing -> firstEnv ns

nonEmpty :: Maybe String -> Maybe String
nonEmpty (Just v) | not (null (filter (/= ' ') v)) = Just (filter (/= ' ') v)
nonEmpty _ = Nothing

safely :: String -> IO () -> IO ()
safely label act = do
  r <- try act
  case r of
    Left e -> hPutStrLn stderr (label <> ": " <> show (e :: SomeException))
    Right () -> pure ()

sendDeck :: Env -> Int -> Deck -> Text -> IO ()
sendDeck env cid deck caption = do
  bytes <- renderDeck templateDir deck
  sendDocument (envClient env) cid "presentation.pptx" bytes caption

generateFor :: Env -> Int -> Int -> Text -> Int -> Theme -> IO ()
generateFor env cid uid topic count theme = case envAi env of
  Nothing -> do
    release st cid
    say c cid "Автоматическая генерация сейчас выключена."
  Just ai -> do
    let limit = if isJust (asAdmin (envAdmins env) uid) then maxBound else envLimit env
    allowed <- reserve st cid limit
    if not allowed
      then do
        release st cid
        say c cid "Лимит автоматических презентаций исчерпан. Можно собрать презентацию вручную: /new"
      else do
        say c cid "Составляю слайды, это займёт около минуты."
        void . forkIO $ do
          r <- try (generate ai topic count)
          case r of
            Left e -> do
              hPutStrLn stderr ("generate: " <> show (e :: SomeException))
              failed "Не удалось связаться с нейросетью. Попробуйте ещё раз позже."
            Right (Left err) -> failed ("Не получилось составить слайды: " <> err)
            Right (Right slides) -> do
              fresh <- adopt st cid topic theme slides
              when fresh . safely "deliver" $ do
                sendDeck env cid (Deck (T.take 80 topic) theme slides) "Ваша презентация готова."
                say c cid "Можно дописать слайды сообщениями, удалить последний через /undo и снова нажать /done. Новая презентация: /auto"
  where
    c = envClient env
    st = envStore env
    failed msg = do
      refund st cid
      release st cid
      safely "notify" (say c cid msg)

serve :: Env -> Message -> IO ()
serve env (Message cid uid (Just txt))
  | txt == "/stats" =
      case asAdmin (envAdmins env) uid of
        Just a -> report a (envStore env) >>= say (envClient env) cid
        Nothing -> say (envClient env) cid "Команда доступна только администраторам."
  | otherwise = do
      out <- converse (isJust (envAi env)) (envStore env) cid txt
      case out of
        Say t buttons -> sayWith (envClient env) cid t buttons
        Deliver deck caption -> sendDeck env cid deck caption
        Generate topic count theme -> generateFor env cid uid topic count theme
serve _ _ = pure ()

nextOffset :: Maybe Int -> [Update] -> Maybe Int
nextOffset off [] = off
nextOffset _ us = Just (maximum (map updateId us) + 1)

loop :: Env -> Maybe Int -> IO ()
loop env off = do
  r <- try (poll (envClient env) off)
  case r of
    Left e -> do
      hPutStrLn stderr ("poll: " <> show (e :: SomeException))
      threadDelay 3000000
      loop env off
    Right us -> do
      forM_ us $ \u ->
        forM_ (incoming u) $ \m -> do
          r' <- try (serve env m)
          case r' of
            Right () -> pure ()
            Left e -> do
              hPutStrLn stderr ("serve: " <> show (e :: SomeException))
              safely "notify" (say (envClient env) (chatId m) "Что-то пошло не так, попробуйте ещё раз.")
      loop env (nextOffset off us)

main :: IO ()
main = do
  token <- lookupEnv "BOT_TOKEN" >>= maybe (die "BOT_TOKEN не задан") pure
  admins <- maybe Set.empty parseAdmins <$> lookupEnv "BOT_ADMINS"
  apiKey <- nonEmpty <$> firstEnv ["AI_API_KEY", "ANTHROPIC_API_KEY"]
  override <- fmap T.pack . nonEmpty <$> firstEnv ["AI_MODEL", "ANTHROPIC_MODEL"]
  limit <- maybe 5 id . (>>= readMaybe) <$> lookupEnv "BOT_AI_LIMIT"
  found <- doesDirectoryExist templateDir
  unless found (die "Не найдена папка template. Запускайте бота из папки проекта.")
  client <- newClient token
  ai <- maybe (pure Nothing) (\k -> newAi k override) apiKey
  store <- newStore
  putStrLn $ case (ai, apiKey) of
    (Just a, _) -> "Автогенерация слайдов включена: " <> T.unpack (describe a)
    (Nothing, Just _) -> "Не удалось определить сервис по ключу. Поддерживаются ключи Groq (gsk_), OpenRouter (sk-or-) и Anthropic (sk-ant-). Работает только ручной режим."
    (Nothing, Nothing) -> "Ключ ИИ не задан, работает только ручной режим."
  safely "configure" (configure client commandList botDescription botShortDescription)
  loop (Env client store admins ai limit) Nothing
