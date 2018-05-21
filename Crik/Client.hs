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
import Web.HttpApiData (ToHttpApiData, toUrlPiece, toQueryParam)

import Crik.API
import Crik.Types
import Crik.Types.Video
import Crik.Types.VideoFile
import Crik.Types.VideoLibrary

instance ToHttpApiData VideoId where
  toUrlPiece (VideoId id) = toUrlPiece id
  toQueryParam (VideoId id) = toQueryParam id

instance ToHttpApiData VideoFileId where
  toUrlPiece (VideoFileId id) = toUrlPiece id
  toQueryParam (VideoFileId id) = toQueryParam id

instance ToHttpApiData VideoLibraryId where
  toUrlPiece (VideoLibraryId id) = toUrlPiece id
  toQueryParam (VideoLibraryId id) = toQueryParam id

-- Video API
proxiedVideoAPI :: Proxy VideoAPI
proxiedVideoAPI = Proxy

getVideo :: VideoId -> ClientM (Video VideoId)
getVideos :: ClientM [Video VideoId]
createVideo :: Video NoId -> ClientM (Video VideoId)
updateVideo :: VideoId -> (Video (Maybe VideoId)) -> ClientM (Video VideoId)
getFilesForVideo :: VideoId -> ClientM [VideoFile VideoFileId]
( getVideo :<|>
  getVideos :<|>
  createVideo :<|>
  updateVideo :<|>
  getFilesForVideo) = client proxiedVideoAPI

-- File API
proxiedFileAPI :: Proxy FileAPI
proxiedFileAPI = Proxy

getFile :: VideoFileId -> ClientM (VideoFile VideoFileId)
getFiles :: ClientM [VideoFile VideoFileId]
createFile :: (VideoFile NoId) -> ClientM (VideoFile VideoFileId)
( getFile :<|>
  getFiles :<|>
  createFile) = client proxiedFileAPI

-- Library API
proxiedLibraryAPI :: Proxy LibraryAPI
proxiedLibraryAPI = Proxy

getLibrary :: VideoLibraryId -> ClientM (VideoLibrary VideoLibraryId)
getLibraries :: ClientM [VideoLibrary VideoLibraryId]
createLibrary :: (VideoLibrary NoId) -> ClientM (VideoLibrary VideoLibraryId)
updateLibrary :: VideoLibraryId -> (VideoLibrary NoId) -> ClientM (VideoLibrary VideoLibraryId)
getNewFilesInLibrary :: VideoLibraryId -> ClientM [Text]
getAllFilesInLibrary :: VideoLibraryId -> ClientM [Text]
( getLibrary :<|>
  getLibraries :<|>
  createLibrary :<|>
  updateLibrary :<|>
  getNewFilesInLibrary :<|>
  getAllFilesInLibrary) = client proxiedLibraryAPI
