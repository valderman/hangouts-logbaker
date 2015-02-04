{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Data.Hangouts.Types where
import qualified Data.Text as T
import qualified Data.Map.Strict as M

-- | Message timestamp.
type Timestamp = T.Text

-- | User identifier.
newtype UID = UID Integer
  deriving (Show, Num, Ord, Eq)

-- | A conversation identifier.
type CID = T.Text

-- | A map of conversation IDs to their messages.
type ConvMap c = M.Map CID [Message c]

-- | User name.
type Name = T.Text

-- | Contents of a text message.
newtype Content = Content [Segment]
  deriving Show

-- | Type of a content segment.
type SegType = T.Text

-- | A text segment.
data Segment = Text T.Text | Other SegType
  deriving Show

-- | A chat message.
data Message content = Message {
    msgFrom      :: !UID,
    msgTimestamp :: !Timestamp,
    msgContent   :: !content
  } deriving (Functor, Show)

-- | A whole Hangouts log, consisting of multiple conversations.
data Log content = Log {
    logNames         :: !(M.Map UID Name),
    logConversations :: !(M.Map CID [Message content])
  } deriving Show

instance Functor Log where
  fmap f (Log ns ms) = Log ns $ M.map (map (fmap f)) ms
