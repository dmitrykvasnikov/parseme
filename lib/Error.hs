{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StrictData         #-}

module Error where

import           Data.Function (on)
import           Pos

type Message = String

-- @Message@ - custom message
-- @System@ - internal messages, with empty Message means absence of Error
-- order of data constructors is order of message importance from high to low
data ErrorType = Unexpected | Expected | Message | System deriving (Enum, Eq)

instance Ord ErrorType where
  compare = on compare fromEnum

data ErrorMessage = ErrorMessage { eType :: ErrorType
                                 , eMsg  :: Message
                                 }

instance Eq ErrorMessage where
  (==) = on (==) eType

instance Ord ErrorMessage where
  compare = on compare eMsg

data ParsemeError = ParsemeError { ePos  :: Pos
                                 , eMsgs :: [ErrorMessage]
                                 }

instance Eq ParsemeError where
  pe1 == pe2 = (on (==) ePos pe1 pe2) && (on (==) eMsgs pe1 pe2)
