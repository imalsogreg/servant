{-# LANGUAGE TypeFamilies #-}

module Servant.Server.Backend (

  ServerBackend (..),

) where

import Servant

class ServerBackend b where

  -- | The base monad
  type ServerMonad b :: * -> *

  type Application b :: *
  type Request b :: *
  type Response b :: *

  type HttpVersion b :: *
  type SockAddr b :: *

  httpVersion :: Request b -> HttpVersion b
  isSecure:: Request b -> Bool

  lazyRequestBody :: Request b -> (ServerMonad b) BL.ByteString

  rawQueryString :: Request b -> BS.ByteString

  remoteHost :: Request b -> SockAddr b

  requestHeaders :: Request b -> [(CI BS.ByteString, BS.ByteString)]

  requestMethod :: Request b -> StdMethod

  responseLBS :: Int -> BS.ByteString -> BL.ByteString -> Response b

  vault :: Request b -> Vault
