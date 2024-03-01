module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
import Data.Maybe (fromJust)

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  let empty_database = DB.empty
  DB.save empty_database
  return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  loaded_database <- DB.load
  case loaded_database of
    Success entry -> handleSuccessCase entry
    Error err -> handleErrorCase err
  where
    handleSuccessCase entry =
      case findEntryById (getOptId getOpts) entry of
        Just found_entry -> putStrLn $ entrySnippet found_entry
        Nothing -> putStrLn "No first entry found with the given id"

    findEntryById givenId = DB.findFirst (\x -> entryId x == givenId)

    handleErrorCase err = putStrLn "Failed to load DB"

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  loaded_database <- DB.load
  case loaded_database of
    Success entries -> handleSuccessCase entries
    Error err -> handleErrorCase err
  where
    handleSuccessCase entries =
      let
        matching_entries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) entries
      in
        case matching_entries of
          [] -> putStrLn "No entries found"
          _ -> sequence_ [print (FmtEntry entry) | entry <- matching_entries]

    handleErrorCase err = putStrLn "Failed to load DB"

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  loaded_database <- DB.load
  src <- readFile (addOptFilename addOpts)
  case loaded_database of
    Success entries -> handleSuccessCase src entries
    Error err -> handleErrorCase err
  where
    handleSuccessCase src entries = 
      let 
        database = DB.insertWith (\id -> makeEntry id src addOpts) entries
        query = DB.findFirst (\element -> entrySnippet element == src) entries
      in 
        case query of
          Just founded_query -> Prelude.mapM_ putStrLn ["Entry with this content already exists: ", show (FmtEntry founded_query)]
          Nothing -> do
            DB.save database 
            return ()

    handleErrorCase err = putStrLn "Failed to load DB"

    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
