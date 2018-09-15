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
    ( runPubOverZmqServer
    )
where
import qualified Control.Concurrent.STM        as STM
import qualified Control.Monad                 as M
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Char8         as B8
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Sequence                 as Seq
import           System.ZMQ4.Monadic                      ( ZMQ )
import qualified System.ZMQ4.Monadic           as ZMQ4

import qualified Conf
import           NetService.ApiTypes                      ( ApiOhlcv )
import qualified NetService.ApiTypes           as ApiTypes

--
publishOverZmq :: ZMQ4.Sender t => ZMQ4.Socket z t -> [ApiOhlcv] -> ZMQ z ()
publishOverZmq sock prices = M.mapM_ (go "" . jsonPrices)
                                     ([ [x] | x <- prices ] ++ [[]])
  where
    --
    go topic contents =
        ZMQ4.sendMulti sock $ NonEmpty.fromList [topic, contents]
    --
    jsonPrices :: [ApiOhlcv] -> B8.ByteString
    jsonPrices = BL8.toStrict . Aeson.encode . Seq.fromList

-- |
--
runPubOverZmqServer :: Conf.Info -> ApiTypes.ServerTChan -> IO ()
runPubOverZmqServer _ chan = ZMQ4.runZMQ $ do
        -- こちらはPublish側なのでbindする
    pubSocket <- ZMQ4.socket ZMQ4.Pub
    ZMQ4.bind pubSocket publishAddr
    --
    M.forever $ do
        msg <- ZMQ4.liftIO . STM.atomically . STM.readTChan $ chan
        publishOverZmq pubSocket msg
    where publishAddr = "tcp://127.0.0.1:8740"


