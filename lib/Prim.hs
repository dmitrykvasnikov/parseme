{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE StrictData             #-}

module Prim where

import           Control.Applicative   (Alternative (..), (<|>))
import           Control.Monad         (MonadPlus (..), ap, mzero, void)
import           Data.Functor.Identity (Identity (runIdentity))
import           Error
import           Pos

-- Stream of tokens with possibility to peek token and return the rest of stream
class (Monad m) => Stream s m t | s -> t where
  peek :: s -> m (Maybe (t, s))

instance (Monad m) => Stream [a] m a where
  peek []      = pure Nothing
  peek (h : t) = pure . Just $ (h, t)

-- State contains input and current position of parser
data State s = State { pInput :: s
                     , pPos   :: SourcePos
                     }

data Consumed a = Consumed a
                | Empty a

instance Functor Consumed where
  fmap f (Consumed a) = Consumed . f $ a
  fmap f (Empty a)    = Empty . f $ a

data Result s a = Ok a (State s) ParsemeError
                | Error ParsemeError

instance Functor (Result s) where
  fmap f (Ok a s err) = Ok (f a) s err
  fmap _ (Error err)  = (Error err)

newtype ParsemeT s m a = ParsemeT { unParseme :: forall b. State s -> (a -> State s -> ParsemeError -> m b) -> (ParsemeError -> m b) -> (a -> State s -> ParsemeError -> m b) -> (ParsemeError -> m b) -> m b }

type Parseme s a = ParsemeT s Identity a

instance Functor (ParsemeT s m) where
  fmap = parsemeMap

instance Applicative (ParsemeT s m) where
  pure = parsemePure
  (<*>) = ap

instance Alternative (ParsemeT s m) where
  empty = mzero
  (<|>) = mplus

instance Monad (ParsemeT s m) where
  return = pure
  (>>=) = parsemeBind

instance MonadPlus (ParsemeT s m) where
  mzero = zero
  mplus = parsemePlus

instance (Semigroup a) => Semigroup (ParsemeT s m a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (ParsemeT s m a) where
  mempty = pure mempty
  mappend = (<>)

runParsemeT :: (Monad m) => ParsemeT s m a -> State s -> m (Consumed (m (Result s a)))
runParsemeT p s = unParseme p s cok cerr eok eerr
  where
    cok a s' = pure . Consumed . pure . Ok a s'
    cerr = pure . Consumed . pure . Error
    eok a s' = pure . Empty . pure . Ok a s'
    eerr = pure . Empty . pure . Error

-- helpers for implementation of Functor / Applicative / Monad instances
parsemeMap :: (a -> b) -> ParsemeT s m a -> ParsemeT s m b
parsemeMap f p = ParsemeT $ \s cok cerr eok eerr ->
  unParseme p s (cok . f) cerr (eok . f) eerr

parsemePure :: a -> ParsemeT s m a
parsemePure a = ParsemeT $ \s _ _ eok _ -> eok a s (unknownError (pPos s))

parsemeBind :: ParsemeT s m a -> (a -> ParsemeT s m b) -> ParsemeT s m b
parsemeBind a f = ParsemeT $ \s cok cerr eok eerr ->
  let acok x s err
        | isUnknownError err = unParseme (f x) s cok cerr cok cerr
        | otherwise =
            let cok' = cok
                cerr' = cerr
                eok' x s err' = cok x s (mergeError err err')
                eerr' err' = cerr (mergeError err err')
             in unParseme (f x) s cok' cerr' eok' eerr'
      acerr = cerr
      aeok x s err
        | isUnknownError err = unParseme (f x) s cok cerr eok eerr
        | otherwise =
            let cok' = cok
                cerr' = cerr
                eok' x s err' = eok x s (mergeError err err')
                eerr' err' = eerr (mergeError err err')
             in unParseme (f x) s cok' cerr' eok' eerr'
      aeerr = eerr
   in -- we provide in new handlers all functionality depending from result of running @a@ parser
      unParseme a s acok acerr aeok aeerr

-- changed from original Parsec to support backtraking by default
parsemePlus :: ParsemeT s m a -> ParsemeT s m a -> ParsemeT s m a
parsemePlus m n = ParsemeT $ \s cok cerr eok eerr ->
  let meerr err =
        let neok x s' err' = eok x s' (mergeError err err')
            neerr err' = eerr (mergeError err err')
         in unParseme n s cok cerr neok neerr
   in unParseme m s cok meerr eok meerr

parseT, runPT :: (Stream s m t) => ParsemeT s m a -> SourceName -> s -> m (Either ParsemeError a)
runPT p n s = do
  res <- runParsemeT p (State s (initPos n))
  parseReply res >>= \case
    (Ok x _ _) -> pure . Right $ x
    (Error e)  -> pure . Left $ e
  where
    parseReply (Consumed a) = a
    parseReply (Empty a)    = a
parseT = runPT

parse, runP :: (Stream s Identity t) => Parseme s a -> SourceName -> s -> Either ParsemeError a
runP p n = runIdentity . runPT p n
parse = runP

-- helper parsers
unexpected :: String -> ParsemeT s m a
unexpected msg = ParsemeT $ \s _ _ _ eerr -> eerr $ newError (pPos s) (UnExpected msg)

failure :: String -> ParsemeT s m a
failure msg = ParsemeT $ \s _ _ _ eerr -> eerr $ newError (pPos s) (Message msg)

zero :: ParsemeT s m a
zero = ParsemeT $ \s _ _ _ eerr -> eerr $ unknownError (pPos s)

try :: ParsemeT s m a -> ParsemeT s m a
try p = ParsemeT $ \s cok _ eok eerr -> unParseme p s cok eerr eok eerr

-- tries use try for backtracing, as with standard alternative @<|>@ operator
tries :: [ParsemeT s m a] -> ParsemeT s m a
tries ps = foldl' (<|>) zero (map try ps)

token :: (Monad m, Stream s m t) => (t -> String) -> (SourcePos -> t -> s -> SourcePos) -> (t -> Maybe a) -> ParsemeT s m a
token showToken nextPos test = ParsemeT $ \(State stream pos) cok _ _ eerr ->
  peek stream >>= \case
    Nothing -> eerr $ newError pos (UnExpected "End of file")
    (Just (t, ts)) -> case test t of
      Nothing -> eerr $ newError pos (UnExpected $ showToken t)
      (Just a) ->
        let nPos = nextPos pos t ts
            nState = State ts nPos
         in seq nPos $ seq nState $ cok a nState (unknownError nPos)

satisfy :: (Stream s m Char) => Char -> ParsemeT s m Char
satisfy c = token (\t -> "'" <> [t] <> "'") (\p c _ -> updateCharPos p c) (\t -> if t == c then (Just c) else Nothing)

string :: (Stream s m Char) => String -> ParsemeT s m String
string = traverse satisfy
