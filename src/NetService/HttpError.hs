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
Module      :  NetService.HttpError
Description :  HTTP error
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

HTTP エラーモジュールです
-}
{-# LANGUAGE StrictData #-}
module NetService.HttpError
    ( err400BadRequest
    , err401Unauthorized
    , err403Forbidden
    , err404NotFound
    , err409Conflict
    , err500InternalServerError
    )
where
import qualified Data.ByteString.Lazy.Char8    as BL8
import qualified Servant

--
--
err400BadRequest :: BL8.ByteString -> Servant.Handler a
err400BadRequest x = Servant.throwError Servant.err400 { Servant.errBody = x }

--
--
err401Unauthorized :: Servant.Handler a
err401Unauthorized = Servant.throwError Servant.err401

--
--
err403Forbidden :: Servant.Handler a
err403Forbidden = Servant.throwError Servant.err403

--
--
err404NotFound :: Servant.Handler a
err404NotFound = Servant.throwError Servant.err404

--
--
err409Conflict :: BL8.ByteString -> Servant.Handler a
err409Conflict x = Servant.throwError Servant.err409 { Servant.errBody = x }

--
--
err500InternalServerError :: Servant.Handler a
err500InternalServerError = Servant.throwError Servant.err500

