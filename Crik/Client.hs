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
, getLibraryByName
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

import Network.HTTP.Client (Response(..))

import Crik.API
import Crik.Types
import Crik.Types.Video
import Crik.Types.File
import Crik.Types.Library

instance ToHttpApiData VideoId where
  toUrlPiece (VideoId id) = toUrlPiece id
  toQueryParam (VideoId id) = toQueryParam id

instance ToHttpApiData FileId where
  toUrlPiece (FileId id) = toUrlPiece id
  toQueryParam (FileId id) = toQueryParam id

instance ToHttpApiData LibraryId where
  toUrlPiece (LibraryId id) = toUrlPiece id
  toQueryParam (LibraryId id) = toQueryParam id

-- Video API
proxiedVideoAPI :: Proxy VideoAPI
proxiedVideoAPI = Proxy

getVideo :: VideoId -> ClientM (Video VideoId)
getVideos :: ClientM [Video VideoId]
createVideo :: Video NoId -> ClientM (Video VideoId)
updateVideo :: VideoId -> (Video (Maybe VideoId)) -> ClientM (Video VideoId)
getFilesForVideo :: VideoId -> ClientM [File FileId]
( getVideo :<|>
  getVideos :<|>
  createVideo :<|>
  updateVideo :<|>
  getFilesForVideo) = client proxiedVideoAPI

-- File API
proxiedFileAPI :: Proxy FileAPI
proxiedFileAPI = Proxy

getFile :: FileId -> ClientM (File FileId)
getFiles :: ClientM [File FileId]
createFile :: (File NoId) -> ClientM (File FileId)
( getFile :<|>
  getFiles :<|>
  createFile) = client proxiedFileAPI

-- Library API
proxiedLibraryAPI :: Proxy LibraryAPI
proxiedLibraryAPI = Proxy

getLibrary :: LibraryId -> ClientM (Library LibraryId)
getLibraryByName :: Text -> ClientM (Library LibraryId)
getLibraries :: ClientM [Library LibraryId]
createLibrary :: (Library NoId) -> ClientM (Library LibraryId)
updateLibrary :: LibraryId -> (Library NoId) -> ClientM (Library LibraryId)
getNewFilesInLibrary :: LibraryId -> ClientM [Text]
getAllFilesInLibrary :: LibraryId -> ClientM [Text]
( getLibrary :<|>
  getLibraryByName :<|>
  getLibraries :<|>
  createLibrary :<|>
  updateLibrary :<|>
  getNewFilesInLibrary :<|>
  getAllFilesInLibrary) = client proxiedLibraryAPI
