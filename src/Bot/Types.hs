module Bot.Types
  ( Count (..)
  , Slides (..)
  , Stage (..)
  , Session (..)
  , SomeSession (..)
  , Slide (..)
  , Theme (..)
  , Deck (..)
  , slidesToList
  , slidesLen
  , beginDeck
  , setTitle
  , setTheme
  , addSlide
  , undoSlide
  , finish
  , beginAuto
  , setTopic
  , setCount
  , startGen
  , loadDeck
  , isCollecting
  , isBusy
  ) where

import Data.Text (Text)

data Count = Z | S Count

data Slide = Slide
  { slideTitle :: Text
  , slideLines :: [Text]
  }

data Theme = Light | Dark | Blue
  deriving (Eq, Enum, Bounded)

data Deck = Deck
  { deckTitle :: Text
  , deckTheme :: Theme
  , deckSlides :: [Slide]
  }

data Slides (n :: Count) where
  Nil  :: Slides 'Z
  Cons :: Slide -> Slides n -> Slides ('S n)

data SomeSlides where
  SomeSlides :: Slides n -> SomeSlides

data Stage
  = Idle
  | WantTitle
  | WantTheme
  | Collecting Count
  | AutoTopic
  | AutoCount
  | AutoTheme
  | Busy

data Session (s :: Stage) where
  SIdle       :: Session 'Idle
  SWantTitle  :: Session 'WantTitle
  SWantTheme  :: Text -> Session 'WantTheme
  SCollecting :: Text -> Theme -> Slides n -> Session ('Collecting n)
  SAutoTopic  :: Session 'AutoTopic
  SAutoCount  :: Text -> Session 'AutoCount
  SAutoTheme  :: Text -> Int -> Session 'AutoTheme
  SBusy       :: Session 'Busy

data SomeSession where
  SomeSession :: Session s -> SomeSession

slidesToList :: Slides n -> [Slide]
slidesToList Nil = []
slidesToList (Cons x xs) = x : slidesToList xs

slidesLen :: Slides n -> Int
slidesLen Nil = 0
slidesLen (Cons _ xs) = 1 + slidesLen xs

beginDeck :: Session 'Idle -> Session 'WantTitle
beginDeck SIdle = SWantTitle

setTitle :: Text -> Session 'WantTitle -> Session 'WantTheme
setTitle t SWantTitle = SWantTheme t

setTheme :: Theme -> Session 'WantTheme -> Session ('Collecting 'Z)
setTheme th (SWantTheme t) = SCollecting t th Nil

addSlide :: Slide -> Session ('Collecting n) -> Session ('Collecting ('S n))
addSlide sl (SCollecting t th ss) = SCollecting t th (Cons sl ss)

undoSlide :: Session ('Collecting ('S n)) -> Session ('Collecting n)
undoSlide (SCollecting t th (Cons _ rest)) = SCollecting t th rest

finish :: Session ('Collecting ('S n)) -> Deck
finish (SCollecting t th ss) = Deck t th (reverse (slidesToList ss))

beginAuto :: Session 'Idle -> Session 'AutoTopic
beginAuto SIdle = SAutoTopic

setTopic :: Text -> Session 'AutoTopic -> Session 'AutoCount
setTopic t SAutoTopic = SAutoCount t

setCount :: Int -> Session 'AutoCount -> Session 'AutoTheme
setCount k (SAutoCount t) = SAutoTheme t k

startGen :: Session 'AutoTheme -> Session 'Busy
startGen (SAutoTheme _ _) = SBusy

loadDeck :: Text -> Theme -> [Slide] -> SomeSession
loadDeck t th sl = case foldl (\(SomeSlides ss) x -> SomeSlides (Cons x ss)) (SomeSlides Nil) sl of
  SomeSlides ss -> SomeSession (SCollecting t th ss)

isCollecting :: SomeSession -> Bool
isCollecting (SomeSession (SCollecting {})) = True
isCollecting _ = False

isBusy :: SomeSession -> Bool
isBusy (SomeSession SBusy) = True
isBusy _ = False
