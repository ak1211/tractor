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
import           Data.Monoid                              ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Builder        as TLB
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
                link_ [rel_ "stylesheet", href_ "https://use.fontawesome.com/releases/v5.3.1/css/all.css", integrity_ "sha384-mzrmE5qonljUremFsqc01SB46JvROS7bZs3IO2EmfFsd15uHvIt+Y8vEf7N7fWAU", crossorigin_ "anonymous"]
                link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css?family=Gugi"]
                link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.2/css/bulma.min.css" ]


            body_ $ do
                script_ [src_ "public/main.js"] T.empty
                script_ [] (TLB.toLazyText launchScript)
        where
        launchScript = "var storageKey = 'store';"
                    <> "var flags = localStorage.getItem(storageKey);"
                    <> "var app = Elm.Main.init({flags: flags});"
                    ----------
                    <> "app.ports.storeCache.subscribe(function(val) {"
                    <> " if (val === null) {"
                    <> "  localStorage.removeItem(storageKey);"
                    <> " } else {"
                    <> "  localStorage.setItem(storageKey, JSON.stringify(val));"
                    <> " }"
                    <> " setTimeout(function() { app.ports.onStoreChange.send(val); }, 0);"
                    <> "});"
                    ----------
                    <> "window.addEventListener('storage', function(event) {"
                    <> " if (event.storageArea === localStorage && event.key === storageKey) {"
                    <> "  app.ports.onStoreChange.send(event.newValue);"
                    <> " }"
                    <> "}, false);"
    toHtmlRaw = Lucid.toHtml

instance Servant.Docs.ToSample HomePage
    where toSamples _ = Servant.Docs.singleSample HomePage

