{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Crik.Client
(
  getVideo
, getVideos
, createVideo
, updateVideo
, getFilesForVideo
, getFile
, getFiles
, createFile
, getLibrary
, getLibraries
, createLibrary
, updateLibrary
, getNewFilesInLibrary
, getAllFilesInLibrary
) where

import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Servant.API ((:<|>)((:<|>)))
import Servant.Client (ClientM, client)

import Crik.API
import Crik.Types
import Crik.Types.Video
import Crik.Types.VideoFile
import Crik.Types.VideoLibrary

-- Video API
proxiedVideoAPI :: Proxy VideoAPI
proxiedVideoAPI = Proxy

getVideo :: Int -> ClientM (Video VideoId)
getVideos :: ClientM [Video VideoId]
createVideo :: Video (Maybe VideoId) -> ClientM (Video VideoId)
updateVideo :: Int -> (Video (Maybe VideoId)) -> ClientM (Video VideoId)
getFilesForVideo :: Int -> ClientM [VideoFile VideoFileId]
( getVideo :<|>
  getVideos :<|>
  createVideo :<|>
  updateVideo :<|>
  getFilesForVideo) = client proxiedVideoAPI

-- File API
proxiedFileAPI :: Proxy FileAPI
proxiedFileAPI = Proxy

getFile :: Int -> ClientM (VideoFile VideoFileId)
getFiles :: ClientM [VideoFile VideoFileId]
createFile :: (VideoFile NoId) -> ClientM (VideoFile VideoFileId)
( getFile :<|>
  getFiles :<|>
  createFile) = client proxiedFileAPI

-- Library API
proxiedLibraryAPI :: Proxy LibraryAPI
proxiedLibraryAPI = Proxy

getLibrary :: Int -> ClientM (VideoLibrary VideoLibraryId)
getLibraries :: ClientM [VideoLibrary VideoLibraryId]
createLibrary :: (VideoLibrary NoId) -> ClientM (VideoLibrary VideoLibraryId)
updateLibrary :: Int -> (VideoLibrary NoId) -> ClientM (VideoLibrary VideoLibraryId)
getNewFilesInLibrary :: Int -> ClientM [Text]
getAllFilesInLibrary :: Int -> ClientM [Text]
( getLibrary :<|>
  getLibraries :<|>
  createLibrary :<|>
  updateLibrary :<|>
  getNewFilesInLibrary :<|>
  getAllFilesInLibrary) = client proxiedLibraryAPI
