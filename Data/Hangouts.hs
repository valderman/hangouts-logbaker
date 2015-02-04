-- | Compile a Google Takeaway-provided Hangouts history into something we
--   can work with. Note that this uses 64 bit integers as user IDs, which is
--   lower precision than what the Takeaway dump uses internally. Collisions
--   may happen in theory, but are completely impossible in practice.
module Data.Hangouts (
    -- * Basic types representing chat logs
    Timestamp, CID, UID (..), Name, Content (..), SegType, Segment (..),
    ConvMap, Message (..), Log (..),
    -- * Parsing logs
    readLog, readLogFile,
    -- * Filtering log messages
    Predicate,
    filterL, inConvoWith, inGroupConvo, from, suchThat, (.&.), (.|.),
    -- * Misc. utility functions
    allMessages,
    textOnly
  ) where
import Control.Exception (try, SomeException)
import Data.Aeson (decode, fromJSON, Result (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Hangouts.Types
import Data.Hangouts.JSON ()

-- | A filter over a chat log.
newtype Predicate a = Predicate (Log a -> CID -> Message a -> Bool)

-- | Convert 'Content' into plaintext, discarding any non-plaintext elements.
textOnly :: Content -> T.Text
textOnly (Content ss) = T.unwords [t | Text t <- ss]

-- | Read a 'Log' from a 'BSL.ByteString'.
readLog :: BSL.ByteString -> Either String (Log Content)
readLog bs =
  case decode bs of
    Just val ->
      case fromJSON val of
        Success l -> Right l
        Error e   -> Left e
    Nothing -> Left "Not valid JSON!"

-- | Read a 'Log' from a file.
readLogFile :: FilePath -> IO (Either String (Log Content))
readLogFile f = do
  edata <- try $ BSL.readFile f
  case edata of
    Right d -> return $ readLog d
    Left e  -> return $ Left (show (e :: SomeException))

-- | All messages from a conversation where the given person participates.
inConvoWith :: Name -> Predicate a
inConvoWith name = Predicate $ \l ->
  case revLookup name (logNames l) of
    Just uid ->
      let cs = allConvosWith uid l
      in \cid _ -> cid `elem` cs
    _        ->
      \_ _ -> False

-- | All messages from a particular person.
from :: Name -> Predicate a
from name = Predicate $ \l _ ->
  case revLookup name (logNames l) of
    Just uid -> \m -> msgFrom m == uid
    _        -> const False

-- | All messages fulfilling the given predicate.
suchThat :: (Message a -> Bool) -> Predicate a
suchThat p = Predicate $ \_ _ m -> p m

-- | All messages from conversations with more than two (active) participants.
inGroupConvo :: Predicate a
inGroupConvo = Predicate $ \l cid _ -> isGroupConvo l cid

isGroupConvo :: Log a -> CID -> Bool
isGroupConvo l cid =
    case M.lookup cid (logConversations l) of
      Just ms -> moreThanTwoPeople S.empty ms
      _       -> False
  where
    moreThanTwoPeople ppl (m:ms) =
      case S.insert (msgFrom m) ppl of
        ppl' | S.size ppl' > 2 -> True
             | otherwise       -> moreThanTwoPeople ppl' ms
    moreThanTwoPeople _ _ =
      False

-- | (Slow!) reverse lookup on a 'M.Map'.
revLookup :: Eq v => v -> M.Map k v -> Maybe k
revLookup v m =
  case filter (\(k, v') -> v == v') (M.toList m) of
    ((k,_):_) -> Just k
    _          -> Nothing

-- | The list of all conversations where a given user participates.
allConvosWith :: UID -> Log a -> [CID]
allConvosWith uid l =
  [fst c | c <- M.toList $ logConversations l, uid `inConvo` c]

-- | Is the given user in the given conversation?
inConvo :: UID -> (CID, [Message c]) -> Bool
inConvo uid (_, ms) = any (\m -> uid == msgFrom m) ms

-- | Filter a conversation based on some criteria.
filterL :: Predicate a -> Log a -> Log a
filterL (Predicate f) l =
    l {logConversations = M.mapWithKey f' (logConversations l)}
  where
    f' cid ms = filter (f l cid) ms

-- | Get all messages from a log.
allMessages :: Log a -> [Message a]
allMessages l = concat $ map snd $ M.toList (logConversations l)

-- | Conjunction of two predicates.
(.&.) :: Predicate a -> Predicate a -> Predicate a
(Predicate a) .&. (Predicate b) = Predicate $ \l c m -> a l c m && b l c m

-- | Disjunction of two predicates.
(.|.) :: Predicate a -> Predicate a -> Predicate a
(Predicate a) .|. (Predicate b) = Predicate $ \l c m -> a l c m || b l c m
