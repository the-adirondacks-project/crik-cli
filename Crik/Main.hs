import Data.Semigroup ((<>))
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Options.Applicative
import Servant.Client

import Crik.Client

data Command = Videos | Files deriving (Show)

commandParser :: Parser Command
commandParser = subparser (
    command "videos" (info (pure Videos) (progDesc "Gets all videos")) <>
    command "files" (info (pure Files) (progDesc "Gets all files"))
  )

parser = info (commandParser <**> helper)
  (fullDesc <> progDesc "A program that does things" <> header "program - a thing")

main :: IO ()
main = execParser parser >>= handleCommand


handleCommand :: Command -> IO ()
handleCommand Videos = do
  manager <- newManager defaultManagerSettings
  response <- runClientM getVideos (mkClientEnv manager (BaseUrl Http "localhost" 8015 ""))
  case response of
    Left error -> print error
    Right videos -> print videos
handleCommand Files = do
  manager <- newManager defaultManagerSettings
  response <- runClientM getFiles (mkClientEnv manager (BaseUrl Http "localhost" 8015 ""))
  case response of
    Left error -> print error
    Right files -> print files
