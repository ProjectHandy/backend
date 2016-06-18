{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import qualified Data.Map.Strict as Map
import GHC.Generics
import Data.Aeson

type UserName = String
type EMailAddress = String
type Password = String

data NotesTaken = Notes Int deriving (Show, Eq, Read)
data PaperQuality = Paper Int deriving (Show, Eq, Read)
type Price = Float

type BookID = String -- bookID refers to the ISBN 
type ID = Int -- this is the unique identifier for each item

type Info = (ID, NotesTaken, PaperQuality, Price)

data PropInfo = PropInfo { date :: String, time :: String, place :: String }
                deriving (Show, Eq, Read, Generic)

data Prop = Prop { id :: ID, buyer :: EMailAddress, seller :: EMailAddress, propInfo :: PropInfo, buyerToSeller :: Bool, chat :: Maybe String }
            deriving (Show, Eq, Read)

type TradeInfo = (Info, [Prop])
type SellInfo = Map.Map EMailAddress Info --for bookDB
type SellerInfo = Map.Map ID TradeInfo -- for UserDB
type BuyerInfo = Map.Map ID TradeInfo

type UserInfo = (UserName, EMailAddress, Password)

type UserDB = Map.Map EMailAddress (UserName, Password, SellerInfo, BuyerInfo)

data BookInfo =
  BookInfo { identifier :: ID, title :: String, number :: Int, lowest :: Float, sellInfo :: SellInfo } deriving (Show, Eq, Read) 

type BookDB = Map.Map BookID BookInfo

data DataBase =
  DataBase { userDB :: UserDB, bookDB :: BookDB } deriving (Show,Eq,Read)
  
initialUserDB :: UserDB
initialUserDB = Map.empty

initialBookDB :: BookDB
initialBookDB = Map.empty

initialDB :: DataBase
initialDB = DataBase { userDB = initialUserDB, bookDB = initialBookDB }

instance FromJSON PropInfo
instance ToJSON PropInfo
