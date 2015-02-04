{-# LANGUAGE StandaloneDeriving,
             GeneralizedNewtypeDeriving,
             OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Hangouts.SQLite (dumpToSQLite, extendSQLite, initSQLite) where
import Data.Hangouts
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import qualified Data.Map.Strict as M
import Control.Monad

deriving instance ToField UID
instance ToField Content where
  toField = toField . textOnly

-- | Initialize an SQLite database for storing Hangouts logs.
initSQLite :: FilePath -> IO ()
initSQLite db = withConnection db $ \sqlite -> do
  execute_ sqlite "DROP TABLE IF EXISTS messages;"
  execute_ sqlite "DROP TABLE IF EXISTS people;"
  execute_ sqlite "DROP TABLE IF EXISTS conversations;"
  createTables sqlite  

-- | Dump zero or more logs into a SQLite database.
--   The resulting database will have three tables: @people@, @conversations@,
--   and @messages@.
--   @people@ maps user identifiers (@id@) to names (@name@).
--
--   @conversations@ maps conversation IDs to Hangouts conversation IDs.
--   (Fields called @id@ and @hangouts_id@))
--
--   @messages@ contains all messages together with the user identifier of
--   whoever wrote them. (Fields called @sender@ and @text@.)
dumpToSQLite :: ToField a => FilePath -> [Log a] -> IO ()
dumpToSQLite db logs = initSQLite db >> extendSQLite db logs

-- | Extend a previous SQLite dump with more logs.
extendSQLite :: ToField a => FilePath -> [Log a] -> IO ()
extendSQLite db logs =
  withConnection db $ \sqlite -> mapM_ (insertLog sqlite) logs

-- | Create the people and text tables.
createTables :: Connection -> IO ()
createTables sqlite = do
  execute_ sqlite userTable
  execute_ sqlite convTable
  execute_ sqlite textTable

-- | Insert a whole 'Log' into a SQLite table
insertLog :: ToField a => Connection -> Log a -> IO ()
insertLog sqlite l = withTransaction sqlite $ do
    mapM_ insertName (M.toList $ logNames l)
    forM_ (M.toList $ logConversations l) $ \(cid, msgs) -> do
      insertConv cid
      cid' <- lastInsertRowId sqlite
      mapM_ (insertText cid') msgs
  where
    insertConv cid =
      execute sqlite "INSERT INTO conversations (hangouts_id) VALUES (?)"
                     [toField cid]

    insertName (uid, name) =
      execute sqlite "INSERT INTO people VALUES (?,?)"
                     [toField uid, toField name]

    insertText cid (Message uid ts text)=
      execute sqlite "INSERT INTO messages VALUES (?,?,?,?)"
                     [toField uid, toField cid, toField ts, toField text]

textTable :: Query
textTable = "CREATE TABLE `messages`\
            \(`sender` INTEGER NOT NULL,\
            \ `conversation` INTEGER NOT NULL,\
            \ `timestamp` INTEGER NOT NULL,\
            \ `text` TEXT NOT NULL,\
            \ FOREIGN KEY(sender) REFERENCES people(id),\
            \ FOREIGN KEY(conversation) REFERENCES conversations(id));"

userTable :: Query
userTable = "CREATE TABLE `people`\
            \(`id` INTEGER NOT NULL PRIMARY KEY,\
            \`name` TEXT NOT NULL);"

convTable :: Query
convTable = "CREATE TABLE `conversations`\
            \(`id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
            \`hangouts_id` TEXT NOT NULL);"

