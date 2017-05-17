{-# LANGUAGE DeriveGeneric          #-}
module Models (
    Person(..)
) where

import GHC.Generics
import Data.Aeson
import Data.Text    as T

data Person = Person { salutation :: T.Text
                     , firstName :: T.Text
                     , otherNames :: [T.Text]
                     , surname :: T.Text 
                     } deriving (Generic, Show)

instance ToJSON Person 
instance FromJSON Person 
