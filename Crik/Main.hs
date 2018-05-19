import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as BS (putStr)
import Data.Semigroup ((<>))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Options.Applicative
import Servant.Client

import Crik.Client
import Crik.CrudCommands
import Crik.Types
import Crik.Types.Video
import Crik.Types.VideoFile
import Crik.Types.VideoLibrary

parser = info (crudCommandParser <**> helper)
  (fullDesc <> progDesc "A program that does things" <> header "program - a thing")

getAPIEnvironment :: IO (ClientEnv)
getAPIEnvironment = do
  manager <- newManager defaultManagerSettings
  return (mkClientEnv manager (BaseUrl Http "localhost" 8015 ""))

main :: IO ()
main = do
  apiEnvironment <- getAPIEnvironment
  execParser parser >>= handleCommand apiEnvironment

handleCommand :: ClientEnv -> CrudCommand -> IO ()
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
