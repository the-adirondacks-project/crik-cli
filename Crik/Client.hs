{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Crik.Client
(
  getAllVideos
) where

import Data.Proxy (Proxy(Proxy))
import Servant.Client
import Servant.API

import Crik.API (GetVideos)
import Crik.Types.Video (Video, VideoId)

type GetVideoAPI = "api" :> (GetVideos)

getVideos :: Proxy GetVideoAPI
getVideos = Proxy

getAllVideos :: ClientM [Video VideoId]
getAllVideos = client getVideos
