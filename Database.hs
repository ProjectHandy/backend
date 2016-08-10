{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import qualified Data.Map.Strict as Map
import GHC.Generics
import Data.Aeson

type UserName = String
type EMailAddress = String
type Password = String

type BookID = String -- bookID refers to the ISBN 
type ID = Int -- this is the unique identifier for each item

data Info = Info { bookid :: ID, note :: Int, paper :: Int, price :: Float} deriving (Show, Eq, Read, Generic)

data PropInfo = PropInfo { date :: String, time :: String, place :: String }
                deriving (Show, Eq, Read, Generic)

data Prop = Prop { id :: ID, buyer :: EMailAddress, seller :: EMailAddress, propInfo :: [PropInfo], buyerToSeller :: Bool, chat :: Maybe String }
            deriving (Show, Eq, Read, Generic)

type TradeInfo = (Info, Maybe Prop)
type SellInfo = (EMailAddress, Info) --for bookDB

--the maybe tradeinfo signifies whether there is an incoming notification
type SellerInfo = (Maybe TradeInfo, Map.Map ID TradeInfo) -- for UserDB
type BuyerInfo = Map.Map ID TradeInfo

data UserInfo = UserInfo {user :: UserName, email :: EMailAddress, pwd :: Password, token :: String} deriving (Show, Eq, Read)

type UserDB = Map.Map EMailAddress (UserInfo, SellerInfo, BuyerInfo)

data BookInfo =
  BookInfo { books :: Map.Map ID SellInfo, isbn :: String, title :: String, author :: String, lowest :: Float, highest :: Float} deriving (Show, Eq,Read) 

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

instance FromJSON Prop
instance ToJSON Prop

instance ToJSON BookInfo where
  toJSON (BookInfo books isbn title author lowest highest) =
    object ["title" .= title, "author" .= author, "isbn" .= isbn]

instance ToJSON Info
