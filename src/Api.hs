{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api (app) where

import Control.Monad.Reader             (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either       (EitherT, left)
import Control.Monad.Trans.Except       (ExceptT)
import Network.Wai                      (Application)
import Data.Int                         (Int64)
import Servant

import Config                           (Config(..), App(..))
import Models
import Api.Person

type CardfileAPI = PersonAPI 

-- | This is the function that will run the full cardfile API
cardfileApp :: Config -> Application
cardfileApp cfg = serve (Proxy :: Proxy CardfileAPI) (appToServer cfg)

cardfileServer :: ServerT CardfileAPI App
cardfileServer = peopleServer

-- | This function tells Servant how to run the App monad with
-- the server function
appToServer :: Config -> Server CardfileAPI
appToServer cfg = enter (convertApp cfg) cardfileServer

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. 
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appApi :: Proxy CardfileAPI
appApi = Proxy

app :: Config -> Application
app cfg = 
    serve appApi $ appToServer cfg