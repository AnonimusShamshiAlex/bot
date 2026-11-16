module Bot.Logic
  ( Role (..)
  , Actor
  , Out (..)
  , asAdmin
  , advance
  , converse
  , report
  , commandList
  , botDescription
  , botShortDescription
  ) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Bot.Store
import Bot.Types

data Role = Member | Admin

newtype Actor (r :: Role) = Actor Int

data Out
  = Say Text [Text]
  | Deliver Deck Text

asAdmin :: Set Int -> Int -> Maybe (Actor 'Admin)
asAdmin ids uid
  | uid `Set.member` ids = Just (Actor uid)
  | otherwise = Nothing

maxSlides :: Int
maxSlides = 20

themeName :: Theme -> Text
themeName Light = "Светлая"
themeName Dark = "Тёмная"
themeName Blue = "Синяя"

themeButtons :: [Text]
themeButtons = map themeName [minBound ..]

themeByName :: Text -> Maybe Theme
themeByName raw = lookup (T.toLower (T.strip raw)) table
  where
    table = concat
      [ [(T.toLower (themeName th), th), (T.pack (show (fromEnum th + 1)), th)]
      | th <- [minBound ..]
      ]

commandList :: [(Text, Text)]
commandList =
  [ ("new", "Создать новую презентацию")
  , ("done", "Собрать и получить файл")
  , ("undo", "Удалить последний слайд")
  , ("cancel", "Отменить текущую презентацию")
  , ("help", "Как пользоваться ботом")
  ]

botDescription :: Text
botDescription =
  "Собираю презентации PowerPoint прямо в Telegram. Пришлите название и текст слайдов, \
  \выберите тему и получите готовый файл .pptx, который открывается в PowerPoint, \
  \Keynote и Google Slides."

botShortDescription :: Text
botShortDescription = "Делаю презентации .pptx из ваших текстов за пару минут"

welcome :: Text
welcome = T.unlines
  [ "Привет! Я собираю презентации PowerPoint из обычного текста."
  , ""
  , "Нажмите /new, придумайте название, выберите тему и присылайте слайды. Когда всё готово, отправьте /done и я пришлю файл."
  , ""
  , "Подробности: /help"
  ]

helpText :: Text
helpText = T.unlines
  [ "Как это работает"
  , ""
  , "1. /new — начать презентацию"
  , "2. Название и тема"
  , "3. Каждое сообщение — один слайд: первая строка это заголовок, остальные строки это пункты"
  , "4. /done — получить файл .pptx"
  , ""
  , "Пример слайда:"
  , "Почему Haskell"
  , "- строгая типизация"
  , "- чистые функции"
  , "- ленивые вычисления"
  , ""
  , "Остальное: /undo удаляет последний слайд, /cancel отменяет презентацию. Максимум слайдов: " <> T.pack (show maxSlides) <> "."
  ]

askTitle :: Text
askTitle = "Как назвать презентацию? Это будет заголовок первого слайда."

askTheme :: Text
askTheme = "Выберите тему оформления."

askSlides :: Text
askSlides = T.unlines
  [ "Отлично. Теперь присылайте слайды: первая строка заголовок, остальные пункты."
  , "Когда закончите, отправьте /done."
  ]

doneText :: Text
doneText = "Готово! Можно добавить ещё слайды и снова нажать /done, а новую презентацию начинает /new."

command :: Text -> Maybe Text
command msg = case T.uncons stripped of
  Just ('/', _) -> Just (T.takeWhile (\c -> c /= ' ' && c /= '@' && c /= '\n') stripped)
  _ -> Nothing
  where
    stripped = T.strip msg

stripMarker :: Text -> Text
stripMarker t = case mapMaybe (`T.stripPrefix` t) ["- ", "• ", "* ", "– "] of
  (rest : _) -> T.strip rest
  [] -> t

parseSlide :: Text -> Either Text Slide
parseSlide msg = case filter (not . T.null) (map T.strip (T.lines msg)) of
  [] -> Left "Слайд пустой. Напишите заголовок и пункты."
  (h : rest)
    | T.length h > 100 -> Left "Заголовок слишком длинный, до 100 символов."
    | length rest > 12 -> Left "Слишком много пунктов, не больше 12 на слайд."
    | any ((> 250) . T.length) rest -> Left "Один из пунктов длиннее 250 символов."
    | otherwise -> Right (Slide h (map stripMarker rest))

advance :: Text -> SomeSession -> (SomeSession, Out)
advance msg cur@(SomeSession s) = case command msg of
  Just "/start" -> (cur, Say welcome ["/new"])
  Just "/help" -> (cur, Say helpText [])
  Just "/new" -> (SomeSession (beginDeck SIdle), Say askTitle [])
  Just "/cancel" -> (SomeSession SIdle, Say "Отменено. Чтобы начать заново, нажмите /new." ["/new"])
  Just "/undo" -> undoLast
  Just "/done" -> finishDeck
  Just _ -> (cur, Say "Не знаю такой команды. Список команд: /help" [])
  Nothing -> plain
  where
    undoLast :: (SomeSession, Out)
    undoLast = case s of
      SCollecting _ _ (Cons _ _) -> (SomeSession (undoSlide s), Say "Последний слайд удалён." [])
      _ -> (cur, Say "Пока нечего удалять." [])

    finishDeck :: (SomeSession, Out)
    finishDeck = case s of
      SCollecting _ _ Nil -> (cur, Say "Сначала добавьте хотя бы один слайд." [])
      SCollecting _ _ (Cons _ _) -> (cur, Deliver (finish s) doneText)
      _ -> (cur, Say "Пока нечего собирать. Начните с /new." ["/new"])

    plain :: (SomeSession, Out)
    plain = case s of
      SIdle -> (cur, Say "Чтобы создать презентацию, нажмите /new." ["/new"])
      SWantTitle ->
        let t = T.strip msg
        in if T.null t || T.length t > 80
             then (cur, Say "Название должно быть от 1 до 80 символов." [])
             else (SomeSession (setTitle t s), Say askTheme themeButtons)
      SWantTheme _ -> case themeByName msg of
        Nothing -> (cur, Say "Выберите тему кнопкой или напишите её название." themeButtons)
        Just th -> (SomeSession (setTheme th s), Say askSlides [])
      SCollecting _ _ ss
        | slidesLen ss >= maxSlides ->
            (cur, Say ("Достигнут предел в " <> T.pack (show maxSlides) <> " слайдов. Отправьте /done.") ["/done"])
        | otherwise -> case parseSlide msg of
            Left e -> (cur, Say e [])
            Right sl ->
              ( SomeSession (addSlide sl s)
              , Say ("Слайд " <> T.pack (show (slidesLen ss + 1)) <> " добавлен. Присылайте следующий или отправьте /done.") ["/done"]
              )

converse :: Store -> Int -> Text -> IO Out
converse st cid msg = atomically $ do
  m <- readTVar (sessions st)
  let cur = Map.findWithDefault (SomeSession SIdle) cid m
      (next, out) = advance msg cur
  writeTVar (sessions st) (Map.insert cid next m)
  case out of
    Deliver _ _ -> modifyTVar' (made st) (+ 1)
    Say _ _ -> pure ()
  pure out

report :: Actor 'Admin -> Store -> IO Text
report (Actor _) st = do
  m <- readTVarIO (sessions st)
  n <- readTVarIO (made st)
  let total = Map.size m
      active = length (filter isCollecting (Map.elems m))
  pure . T.unlines $
    [ "Диалогов: " <> T.pack (show total)
    , "Собирают сейчас: " <> T.pack (show active)
    , "Презентаций выдано: " <> T.pack (show n)
    ]
