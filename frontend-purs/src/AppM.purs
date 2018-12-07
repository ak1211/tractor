module AppM
  ( AppM(..)
  , class SessionDSL
  , runAppM
  , getSession
  ) where


import Prelude

import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, lift)
import Session(Session)
import Type.Equality as TE


-- AppM
newtype AppM a = AppM (ReaderT Session Aff a)


derive instance newtypeAppM :: Newtype (AppM a) _


derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM


-- HELPER


runAppM :: forall a. AppM a -> Session -> Aff a
runAppM m session =
  runReaderT (unwrap m) session


-- MonadAsk INSTANCES
instance monadAskAppM :: TE.TypeEquals e Session => MonadAsk e AppM where
  ask = AppM $ asks TE.from


-- SessionDSL


class Monad m <= SessionDSL m where
  getSession :: m Session


instance sessionDSLAppM :: SessionDSL AppM where
  getSession = AppM ask


instance sessionDSLHalogenM :: SessionDSL m => SessionDSL (HalogenM s f g p o m) where
  getSession = lift getSession
