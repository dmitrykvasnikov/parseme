module Source where

import           Data.List ((!?))
import           Pos

-- @showSource@ - print whole source
-- @showLine@ - print concrete line (e.c. error reports)
class Source a where
  showSource :: a -> String
  showLine :: Pos -> a -> String

instance Source [Char] where
  showSource = show
  showLine (Pos l _) s = maybe "" id (lines s !? (l - 1))
