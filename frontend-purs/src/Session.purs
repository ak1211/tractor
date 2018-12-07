module Session
  ( Session(..)
  , showUsername
  ) where


import Prelude

import Api (Cred, credUsername)
import Data.Maybe (Maybe(..))


-- Session
data Session = LoggedIn Cred | Guest


showUsername :: Session -> Maybe String
showUsername (LoggedIn cred) = Just $ credUsername cred
showUsername Guest = Nothing
