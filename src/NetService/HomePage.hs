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
Module      :  NetService.HomePage
Description :  Home page of this application front end
Copyright   :  (c) 2016 Akihiro Yamamoto
License     :  AGPLv3

Maintainer  :  https://github.com/ak1211
Stability   :  unstable
Portability :  POSIX

ホームページです
-}
{-# LANGUAGE OverloadedStrings #-}
module NetService.HomePage
    ( HomePage(..)
    )
where
import qualified Data.Text                     as T
import           Lucid
import qualified Servant.Docs

-- |
-- アプリケーションのホームページ
data HomePage = HomePage

instance Lucid.ToHtml HomePage where
    --
    --
    toHtml _ = do
        doctype_
        html_ [lang_ "ja", class_ "has-navbar-fixed-top"] $ do
            head_ $ do
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
                title_ $ Lucid.toHtmlRaw ("Dashboard &#8212; TRACTOR" :: T.Text)
                link_ [rel_ "icon", href_"public/favicon.ico"]
                link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.2/css/bulma.min.css" ]
                link_ [rel_ "stylesheet", href_ "https://use.fontawesome.com/releases/v5.5.0/css/all.css", integrity_ "sha384-B4dIYHKNBt8Bc12p+WXckhzcICo0wtJAoU8YZTY5qE0Id1GSseTk6S+L3BlXeVIU", crossorigin_ "anonymous"]
                link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Gugi"]
            body_ $ do
                script_ [src_ "public/app.js"] T.empty
    toHtmlRaw = Lucid.toHtml

instance Servant.Docs.ToSample HomePage
    where toSamples _ = Servant.Docs.singleSample HomePage

