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
Module      :  NetService.ServerConfig
Description :  Web API server configuration
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

Web API サーバー設定情報です
-}
{-# LANGUAGE StrictData #-}
module NetService.WebServerConfig
    ( Config(..)
    )
where
import qualified Database.Persist.MySQL        as MySQL
import qualified Servant.Auth.Server

import qualified Conf
import qualified NetService.ApiTypes           as ApiTypes

-- |
-- このサーバーで使う設定情報
data Config = Config
    { cVerRev :: ApiTypes.VerRev
    , cJWTS   :: Servant.Auth.Server.JWTSettings
    , cConf   :: Conf.Info
    , cPool   :: MySQL.ConnectionPool
    , cChan   :: ApiTypes.ServerTChan
    }

