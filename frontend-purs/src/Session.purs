module Session
  ( Session(..)
  ) where


import Api (Cred)


-- Session
data Session = LoggedIn Cred | Guest
