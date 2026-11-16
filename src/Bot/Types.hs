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
  , isCollecting
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

data Stage = Idle | WantTitle | WantTheme | Collecting Count

data Session (s :: Stage) where
  SIdle       :: Session 'Idle
  SWantTitle  :: Session 'WantTitle
  SWantTheme  :: Text -> Session 'WantTheme
  SCollecting :: Text -> Theme -> Slides n -> Session ('Collecting n)

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

isCollecting :: SomeSession -> Bool
isCollecting (SomeSession (SCollecting {})) = True
isCollecting _ = False
