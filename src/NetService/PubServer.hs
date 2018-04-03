{-
    This file is part of Tractor.

    Tractor is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Tractor is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with Tractor.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
Module      :  NetService.PubServer
Description :  pub / sub pattern service, infrastructure with ZeroMQ
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

>>> :set -XOverloadedStrings
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
module NetService.PubServer
    ( module NetService.Types
    , runPubServer
    ) where
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan (readTChan)
import qualified Control.Monad                as M
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy.Char8   as BL8
import qualified Data.List.NonEmpty           as NE
import qualified Data.Sequence                as Seq
import           System.ZMQ4.Monadic          (Sender, Socket, ZMQ)
import qualified System.ZMQ4.Monadic          as ZMQ4

import qualified Conf
import           NetService.Types

--
publish :: Sender t => Socket z t -> [ReplyOhlcv] -> ZMQ z ()
publish sock prices =
    M.mapM_ (go "" . jsonPrices) ([ [x] | x<-prices ] ++ [ [] ])
    where
    --
    go topic contents =
        ZMQ4.sendMulti sock $ NE.fromList [topic, contents]
    --
    jsonPrices :: [ReplyOhlcv] -> B8.ByteString
    jsonPrices =
        BL8.toStrict . Aeson.encode . Seq.fromList

-- |
--
runPubServer :: Conf.Info -> ServerTChan -> IO ()
runPubServer _ chan =
    ZMQ4.runZMQ $ do
        -- こちらはPublish側なのでbindする
        pubSocket <- ZMQ4.socket ZMQ4.Pub
        ZMQ4.bind pubSocket publishAddr
        --
        M.forever $ do
            msg <- ZMQ4.liftIO . atomically . readTChan $ chan
            publish pubSocket msg
    where
    publishAddr = "tcp://127.0.0.1:8740"


