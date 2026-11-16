module Bot.Store (Store (..), newStore) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Bot.Types

data Store = Store
  { sessions :: TVar (Map Int SomeSession)
  , made :: TVar Int
  }

newStore :: IO Store
newStore = Store <$> newTVarIO Map.empty <*> newTVarIO 0
