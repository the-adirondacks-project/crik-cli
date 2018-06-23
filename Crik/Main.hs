{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson (ToJSON, encode)
import Data.Text (Text, pack)
import qualified Data.Text.IO as T (putStrLn)
import qualified Data.ByteString.Lazy as BS (putStr)
import Data.Semigroup ((<>))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Options.Applicative
import Servant.Client

import Crik.Client
import Crik.CrudCommands
import Crik.Commands
import Crik.Commands.Types
import qualified Crik.Commands.Library.Types as CL
import Crik.Types
import Crik.Types.Library

getAPIEnvironment :: IO (ClientEnv)
getAPIEnvironment = do
  manager <- newManager defaultManagerSettings
  return (mkClientEnv manager (BaseUrl Http "localhost" 8015 ""))

main :: IO ()
main = do
  apiEnvironment <- getAPIEnvironment
  execParser commandsParser >>= (flip handleCommand) apiEnvironment

run :: ToJSON a => ClientM a -> ClientEnv -> IO ()
run clientFunction environment = runClientM clientFunction environment >>= handleResponse

handleCommand :: Command -> ClientEnv -> IO ()
handleCommand (CrudCommand crudCommand) environment = handleCrudCommand crudCommand environment
handleCommand (LibraryCommand (CL.LibraryCreate (CL.LibraryCreateOptions{..}))) environment = do
  let url = getUrlForLibrary libraryType libraryLocation
  let library = Library NoId url libraryName
  response <- runClientM (createLibrary library) environment
  case response of
    Left error -> print error
    Right library -> putStrLn "Library created."
handleCommand (LibraryCommand (CL.LibraryList _)) environment = do
  response <- runClientM getLibraries environment
  case response of
    Left error -> print error
    Right libraries -> do
      putStrLn "Name - Type - Location"
      mapM (\library@Library{..} -> do
             T.putStrLn
              (libraryName <> " - " <> (pack $ show (libraryType library)) <> " - " <> libraryUrl)
           ) libraries
      return ()
handleCommand (LibraryCommand (CL.LibraryInfo CL.LibraryInfoOptions{..})) environment = do
  response <- runClientM (getLibraryByName libraryName) environment
  case response of
    Left error -> print error
    Right library@Library{..} -> do
      T.putStrLn $ "Name: " <> libraryName
      putStrLn $ "Type: " <> (show (libraryType library))
      T.putStrLn $ "Location: " <> libraryUrl

-- TODO: Parse location to handle urls better
getUrlForLibrary :: LibraryType -> Text -> Text
getUrlForLibrary HTTP location = "http://" <> location
getUrlForLibrary Directory location = "file://" <> location

handleCrudCommand :: CrudCommand -> ClientEnv -> IO ()
-- Videos
handleCrudCommand (VideoCrudCommand (Get id)) = run (getVideo id)
handleCrudCommand (VideoCrudCommand GetAll) = run getVideos
handleCrudCommand (VideoCrudCommand (Create video)) = run $ createVideo video

-- Files
handleCrudCommand (FileCrudCommand (Get id)) = run (getFile id)
handleCrudCommand (FileCrudCommand GetAll) = run getFiles
handleCrudCommand (FileCrudCommand (Create file)) = run $ createFile file
handleCrudCommand (FileCrudCommand (Delete id)) = undefined

-- Libraries
handleCrudCommand (LibraryCrudCommand (Get id)) = run (getLibrary id)
handleCrudCommand (LibraryCrudCommand GetAll) = run getLibraries
handleCrudCommand (LibraryCrudCommand (Create library)) = run $ createLibrary library

handleResponse :: ToJSON a => Either ServantError a -> IO ()
handleResponse (Left error) = print error
handleResponse (Right x) = do
  BS.putStr (encode x)
  putStrLn ""
