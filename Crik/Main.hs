import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client

import Crik.Client

main :: IO ()
main = do
  print "Hello World"
  manager <- newManager defaultManagerSettings
  response <- runClientM getVideos (mkClientEnv manager (BaseUrl Http "localhost" 8015 ""))
  case response of
    Left error -> print error
    Right videos -> print videos
