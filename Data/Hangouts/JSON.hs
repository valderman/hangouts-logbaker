{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
-- | FromJSON instances for Hangouts data.
module Data.Hangouts.JSON () where
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.List (foldl')
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Hangouts.Types

instance FromJSON UID where
  parseJSON (String s) =
    case reads $ T.unpack s of
      [(n, "")] -> return $ UID n
      _         -> fail "Couldn't parse UID from String!"
  parseJSON _ = fail "Tried to parse UID from non-String!"

instance (FromJSON c) => FromJSON (Log c) where
  parseJSON (Object o) = do
    cs <- o .: "conversation_state"
    tbl <- foldM participantTable M.empty cs
    convmap <- foldM conversation M.empty cs
    return $ Log tbl convmap
  parseJSON _ = error "Tried to parse Log from non-Object!"

-- | Parse a conversation into a list of messages.
conversation :: FromJSON c => ConvMap c -> Value -> Parser (ConvMap c)
conversation cm (Object o) = do
  Object cs <- o .: "conversation_state"
  Array es <- cs .: "event"
  foldM message cm (V.toList es)
conversation _ _ =
  fail "Tried to parse conversation from non-Object!"

-- | Produce a lookup table of fallback names from the given JSON object.
participantTable :: M.Map UID Name -> Value -> Parser (M.Map UID Name)
participantTable table (Object o) = do
  Object cs <- o .: "conversation_state"
  Object conv <- cs .: "conversation"
  Array ps <- conv .: "participant_data"
  ns <- forM (V.toList ps) $ \p -> do
    case p of
      Object o' -> do
        pid <- o' .: "id"
        cid <- pid .: "chat_id"
        mname <- o' .:? "fallback_name"
        case mname of
          Just name -> return $ Just (cid, name)
          _         -> return Nothing
      _ -> do
        return Nothing
  return $ foldl' (\m (k, v) -> M.insert k v m) table $ catMaybes ns
participantTable _ _ =
  fail "Tried to parse participant table from non-Object!"

-- | Parse a Message, substituting its sender ID for a readable name, if that
--   ID exists in the provided lookup table.
message :: (FromJSON c) => ConvMap c -> Value -> Parser (ConvMap c)
message cm (Object o) = do
  msid <- o .:? "sender_id"
  mmsg <- o .:? "chat_message"
  case (msid, mmsg) of
    (Just (Object sid), Just (Object msg)) -> do
      Object cnt <- msg .: "message_content"
      mseg <- cnt .:? "segment"
      case mseg of
        Just seg -> do
          Object cid <- o .: "conversation_id"
          cid' <- cid .: "id"
          extendConvMap cm cid' <$> (Message <$> sid .: "chat_id"
                                             <*> o   .: "timestamp"
                                             <*> pure seg)
        _ -> do
          return cm
    _ -> do
      return cm
message _ _ =
  fail "Tried to parse Message from non-Object!"

instance FromJSON Content where
  parseJSON (Array ms) = Content . catMaybes <$> mapM parseJSON (V.toList ms)
  parseJSON _ = fail "Tried to parse Content from non-Array!"

instance FromJSON Segment where
  parseJSON (Object o) = do
    t <- o .: "type"
    case t of
      "TEXT" -> Text <$> o .: "text"
      _      -> return $ Other t
  parseJSON _ = fail "Tried to parse Segment from non-Object!"

-- | Extend a conversation map with a new message in a particular conversation.
extendConvMap :: ConvMap c -> CID -> Message c -> ConvMap c
extendConvMap cs cid msg =
  M.alter (maybe (Just [msg]) (\xs -> Just $ msg : xs)) cid cs
