import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as BS (putStr)
import Data.Semigroup ((<>))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Options.Applicative
import Servant.Client

import Crik.Client
import Crik.Types
import Crik.Types.Video
import Crik.Types.VideoFile
import Crik.Types.VideoLibrary

data Command =
  VideoCommand (APISubCommand (Video NoId) VideoId) |
  FileCommand (APISubCommand (VideoFile NoId) VideoFileId) |
  LibraryCommand (APISubCommand (VideoLibrary NoId) VideoLibraryId)
  deriving (Show)

data APISubCommand item id = Get id | GetAll | Create item | Update item deriving (Show)

commandParser :: Parser Command
commandParser =
  subparser (
    command "videos" (info (VideoCommand <$> videosParser) (progDesc "Foo")) <>
    command "files" (info (pure $ FileCommand GetAll) (progDesc "Gets all files")) <>
    command "libraries" (info (pure $ LibraryCommand GetAll) (progDesc "Gets all files"))
  )

videosParser :: Parser (APISubCommand (Video NoId) VideoId)
videosParser = subparser (
    command "list" (info (pure GetAll) (progDesc "Lists all videos")) <>
    command "add" (info (Create <$> addVideoParser) (progDesc "Adds a video"))
  ) <|> (pure GetAll)

addVideoParser :: Parser (Video NoId)
addVideoParser = Video NoId <$> strOption (
    long "name" <>
    metavar "NAME" <>
    help "Name for video"
  )

parser = info (commandParser <**> helper)
  (fullDesc <> progDesc "A program that does things" <> header "program - a thing")

getAPIEnvironment :: IO (ClientEnv)
getAPIEnvironment = do
  manager <- newManager defaultManagerSettings
  return (mkClientEnv manager (BaseUrl Http "localhost" 8015 ""))

main :: IO ()
main = do
  apiEnvironment <- getAPIEnvironment
  execParser parser >>= handleCommand apiEnvironment

handleCommand :: ClientEnv -> Command -> IO ()
handleCommand environment (VideoCommand GetAll) =
  runClientM getVideos environment >>= handleResponse
handleCommand environment (VideoCommand (Create video)) =
  runClientM (createVideo video) environment >>= handleResponse
handleCommand environment (FileCommand GetAll) = runClientM getFiles environment >>= handleResponse
handleCommand environment (LibraryCommand GetAll) =
  runClientM getLibraries environment >>= handleResponse

handleResponse :: ToJSON a => Either ServantError a -> IO ()
handleResponse (Left error) = print error
handleResponse (Right x) = do
  BS.putStr (encode x)
  putStrLn ""
