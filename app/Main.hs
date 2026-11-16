module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import Bot.Api
import Bot.Logic
import Bot.Pptx
import Bot.Store

templateDir :: FilePath
templateDir = "template"

parseAdmins :: String -> Set Int
parseAdmins = Set.fromList . mapMaybe readMaybe . words . map (\ch -> if ch == ',' then ' ' else ch)

serve :: Client -> Store -> Set Int -> Message -> IO ()
serve c st admins (Message cid uid (Just txt))
  | txt == "/stats" =
      case asAdmin admins uid of
        Just a -> report a st >>= say c cid
        Nothing -> say c cid "Команда доступна только администраторам."
  | otherwise = do
      out <- converse st cid txt
      case out of
        Say t buttons -> sayWith c cid t buttons
        Deliver deck caption -> do
          bytes <- renderDeck templateDir deck
          sendDocument c cid "presentation.pptx" bytes caption
serve _ _ _ _ = pure ()

safely :: String -> IO () -> IO ()
safely label act = do
  r <- try act
  case r of
    Left e -> hPutStrLn stderr (label <> ": " <> show (e :: SomeException))
    Right () -> pure ()

nextOffset :: Maybe Int -> [Update] -> Maybe Int
nextOffset off [] = off
nextOffset _ us = Just (maximum (map updateId us) + 1)

loop :: Client -> Store -> Set Int -> Maybe Int -> IO ()
loop c st admins off = do
  r <- try (poll c off)
  case r of
    Left e -> do
      hPutStrLn stderr ("poll: " <> show (e :: SomeException))
      threadDelay 3000000
      loop c st admins off
    Right us -> do
      forM_ us $ \u ->
        forM_ (incoming u) $ \m -> do
          r' <- try (serve c st admins m)
          case r' of
            Right () -> pure ()
            Left e -> do
              hPutStrLn stderr ("serve: " <> show (e :: SomeException))
              safely "notify" (say c (chatId m) "Что-то пошло не так, попробуйте ещё раз.")
      loop c st admins (nextOffset off us)

main :: IO ()
main = do
  token <- lookupEnv "BOT_TOKEN" >>= maybe (die "BOT_TOKEN не задан") pure
  admins <- maybe Set.empty parseAdmins <$> lookupEnv "BOT_ADMINS"
  found <- doesDirectoryExist templateDir
  unless found (die "Не найдена папка template. Запускайте бота из папки проекта.")
  client <- newClient token
  store <- newStore
  safely "configure" (configure client commandList botDescription botShortDescription)
  loop client store admins Nothing
