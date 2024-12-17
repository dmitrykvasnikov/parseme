{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData             #-}

module Prim where

import           Error
import           Pos

type Offset = Int

class Stream s t where
  peek :: s -> Maybe (t, s)

instance Stream [a] a where
  peek []      = Nothing
  peek (t : s) = Just (t, s)

data Source s = Source { pSrc :: s
                       , pPos :: Pos
                       }

data Consumed r = Consumed r
                | Empty r

data Result s r = Ok r (Source s) ParsemeError
                | Error ParsemeError

data ParsemeT s m a = ParsemeT { unParseme :: forall b. Source s -> (a -> Source s -> ParsemeError -> m b) -> (ParsemeError -> m b) -> (a -> Source s -> ParsemeError -> m b) -> (ParsemeError -> m b)
                               }

runParsemeT :: (Stream s t, Monad m) => Source s -> ParsemeT s m a -> Consumed (Result s a)
runParsemeT = undefined
