module Bot.Logic
  ( Role (..)
  , Actor
  , Out (..)
  , asAdmin
  , advance
  , converse
  , adopt
  , release
  , reserve
  , refund
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
import Text.Read (readMaybe)

import Bot.Store
import Bot.Types

data Role = Member | Admin

newtype Actor (r :: Role) = Actor Int

data Out
  = Say Text [Text]
  | Deliver Deck Text
  | Generate Text Int Theme

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

countButtons :: [Text]
countButtons = ["5", "8", "12"]

themeByName :: Text -> Maybe Theme
themeByName raw = lookup (T.toLower (T.strip raw)) table
  where
    table = concat
      [ [(T.toLower (themeName th), th), (T.pack (show (fromEnum th + 1)), th)]
      | th <- [minBound ..]
      ]

commandList :: [(Text, Text)]
commandList =
  [ ("auto", "Бот сам напишет презентацию по теме")
  , ("new", "Собрать презентацию вручную")
  , ("done", "Получить файл с презентацией")
  , ("undo", "Удалить последний слайд")
  , ("cancel", "Отменить текущую презентацию")
  , ("help", "Как пользоваться ботом")
  ]

botDescription :: Text
botDescription =
  "Делаю презентации PowerPoint прямо в Telegram. Напишите тему, и я сам составлю слайды \
  \и пришлю готовый файл .pptx. Можно и собрать слайды вручную. Файл открывается в \
  \PowerPoint, Keynote и Google Slides."

botShortDescription :: Text
botShortDescription = "Напишите тему и получите презентацию .pptx"

welcome :: Bool -> Text
welcome True = T.unlines
  [ "Привет! Я делаю презентации PowerPoint."
  , ""
  , "Просто напишите тему, например «История Ташкента», и я сам составлю слайды и пришлю файл .pptx."
  , ""
  , "Хотите написать слайды сами? Команда /new. Подробности: /help"
  ]
welcome False = T.unlines
  [ "Привет! Я собираю презентации PowerPoint из обычного текста."
  , ""
  , "Нажмите /new, придумайте название, выберите тему и присылайте слайды. Когда всё готово, отправьте /done и я пришлю файл."
  , ""
  , "Подробности: /help"
  ]

helpText :: Bool -> Text
helpText autoOn = T.unlines $
  [ "Как это работает"
  , ""
  ]
  <> (if autoOn
        then
          [ "Автоматически: напишите тему (или /auto), выберите число слайдов и тему оформления. Я составлю слайды и пришлю файл. Потом их можно дописать вручную."
          , ""
          ]
        else [])
  <>
  [ "Вручную: /new, название, тема, затем каждое сообщение это один слайд. Первая строка заголовок, остальные строки пункты. В конце /done."
  , ""
  , "Пример слайда:"
  , "Почему Haskell"
  , "- строгая типизация"
  , "- чистые функции"
  , "- ленивые вычисления"
  , ""
  , "/undo удаляет последний слайд, /cancel отменяет презентацию. Максимум слайдов: " <> T.pack (show maxSlides) <> "."
  ]

askTitle :: Text
askTitle = "Как назвать презентацию? Это будет заголовок первого слайда."

askTopic :: Text
askTopic = "О чём будет презентация? Напишите тему."

askCount :: Text
askCount = "Сколько слайдов сделать? Выберите кнопкой или напишите число от 3 до 15."

askTheme :: Text
askTheme = "Выберите тему оформления."

askSlides :: Text
askSlides = T.unlines
  [ "Отлично. Теперь присылайте слайды: первая строка заголовок, остальные пункты."
  , "Когда закончите, отправьте /done."
  ]

doneText :: Text
doneText = "Готово! Можно добавить ещё слайды и снова нажать /done, а новую презентацию начинает /new."

noAuto :: Text
noAuto = "Автоматическая генерация сейчас выключена. Соберите презентацию вручную: /new"

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

advance :: Bool -> Text -> SomeSession -> (SomeSession, Out)
advance autoOn msg cur@(SomeSession s) = case command msg of
  Just "/start" -> (cur, Say (welcome autoOn) (if autoOn then [] else ["/new"]))
  Just "/help" -> (cur, Say (helpText autoOn) [])
  Just "/new" -> (SomeSession (beginDeck SIdle), Say askTitle [])
  Just "/auto"
    | autoOn -> (SomeSession (beginAuto SIdle), Say askTopic [])
    | otherwise -> (cur, Say noAuto ["/new"])
  Just "/cancel" -> (SomeSession SIdle, Say "Отменено. Чтобы начать заново, напишите тему или нажмите /new." ["/new"])
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
      SBusy -> (cur, Say "Презентация ещё генерируется, подождите немного." [])
      _ -> (cur, Say "Пока нечего собирать. Начните с /new." ["/new"])

    acceptTopic :: Session 'AutoTopic -> (SomeSession, Out)
    acceptTopic p
      | T.length t < 3 || T.length t > 200 = (cur, Say "Тема должна быть от 3 до 200 символов." [])
      | otherwise = (SomeSession (setTopic t p), Say askCount countButtons)
      where
        t = T.strip msg

    plain :: (SomeSession, Out)
    plain = case s of
      SIdle
        | autoOn -> acceptTopic (beginAuto s)
        | otherwise -> (cur, Say "Чтобы создать презентацию, нажмите /new." ["/new"])
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
      SAutoTopic -> acceptTopic s
      SAutoCount _ -> case readMaybe (T.unpack (T.strip msg)) of
        Just k | k >= 3 && k <= 15 -> (SomeSession (setCount k s), Say askTheme themeButtons)
        _ -> (cur, Say "Нужно число от 3 до 15." countButtons)
      SAutoTheme topic k -> case themeByName msg of
        Nothing -> (cur, Say "Выберите тему кнопкой или напишите её название." themeButtons)
        Just th -> (SomeSession (startGen s), Generate topic k th)
      SBusy -> (cur, Say "Презентация ещё генерируется, подождите немного." [])

converse :: Bool -> Store -> Int -> Text -> IO Out
converse autoOn st cid msg = atomically $ do
  m <- readTVar (sessions st)
  let cur = Map.findWithDefault (SomeSession SIdle) cid m
      (next, out) = advance autoOn msg cur
  writeTVar (sessions st) (Map.insert cid next m)
  case out of
    Deliver _ _ -> modifyTVar' (made st) (+ 1)
    _ -> pure ()
  pure out

adopt :: Store -> Int -> Text -> Theme -> [Slide] -> IO Bool
adopt st cid topic th sl = atomically $ do
  m <- readTVar (sessions st)
  case Map.lookup cid m of
    Just cur | isBusy cur -> do
      writeTVar (sessions st) (Map.insert cid (loadDeck (T.take 80 topic) th sl) m)
      modifyTVar' (made st) (+ 1)
      pure True
    _ -> pure False

release :: Store -> Int -> IO ()
release st cid = atomically $
  modifyTVar' (sessions st) (Map.adjust (\c -> if isBusy c then SomeSession SIdle else c) cid)

reserve :: Store -> Int -> Int -> IO Bool
reserve st cid limit = atomically $ do
  u <- readTVar (usage st)
  let n = Map.findWithDefault 0 cid u
  if n >= limit
    then pure False
    else do
      writeTVar (usage st) (Map.insert cid (n + 1) u)
      pure True

refund :: Store -> Int -> IO ()
refund st cid = atomically $ modifyTVar' (usage st) (Map.adjust (\n -> max 0 (n - 1)) cid)

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
