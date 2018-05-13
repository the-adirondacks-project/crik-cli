import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as BS (putStr)
import Data.Semigroup ((<>))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Options.Applicative
import Servant.Client

import Crik.Client

data Command = Videos VideoCommand | Files | Libraries deriving (Show)
data VideoCommand = ListVideos deriving (Show)

commandParser :: Parser Command
commandParser =
  subparser (
    command "videos" (info (Videos <$> videosParser) (progDesc "Foo")) <>
    command "files" (info (pure Files) (progDesc "Gets all files")) <>
    command "libraries" (info (pure Libraries) (progDesc "Gets all files"))
  )

videosParser :: Parser VideoCommand
videosParser = subparser (
    command "list" (info (pure ListVideos) (progDesc "Lists all videos"))
  ) <|> (pure ListVideos)

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
handleCommand environment (Videos ListVideos) = runClientM getVideos environment >>= handleResponse
handleCommand environment Files = runClientM getFiles environment >>= handleResponse
handleCommand environment Libraries = runClientM getLibraries environment >>= handleResponse

handleResponse :: ToJSON a => Either ServantError a -> IO ()
handleResponse (Left error) = print error
handleResponse (Right x) = do
  BS.putStr (encode x)
  putStrLn ""
