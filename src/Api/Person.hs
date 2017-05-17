{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}

module Api.Person (
      PersonAPI
    , peopleServer
) where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)

import           Network.Wai                 (Application)
import           Servant

import           Config                      (App (..), Config (..))
import           Models 

type PersonAPI =
         "people" :>                           Get '[JSON] [Person]
    -- :<|> "people" :> Capture "name" String  :> Get '[JSON] Person
    -- :<|> "people" :> ReqBody '[JSON] Person :> Post '[JSON] Int64

peopleServer :: ServerT PersonAPI App
peopleServer = allPeople

peopleSample = [Person { salutation = "Mr"
                       , firstName = "John"
                       , otherNames = []
                       , surname = "Smith"
                       }]

-- | Returns all people in the database.
allPeople :: App [Person]
allPeople = return peopleSample

