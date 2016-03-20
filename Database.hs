module Database where

import qualified Data.Map.Strict as Map

type UserName = String
type EMailAddress = String
type Password = String

data NotesTaken = None | Few | Heavy deriving (Show, Eq)
data PaperQuality = Poor | Average | Good deriving (Show, Eq)
type Price = Float

type BookName = String

type SellerInfo = Map.Map BookName (NotesTaken, PaperQuality, Price)
type UserInfo = (UserName, EMailAddress, Password)

type UserDB = Map.Map UserName (EMailAddress, Password, SellerInfo)

data BookInfo =
  BookInfo { isbn :: String, number :: Int, lowest :: Float, sellInfo :: SellerInfo } deriving (Show, Eq) 

type BookDB = Map.Map BookName BookInfo

data DataBase =
  DataBase { userDB :: UserDB, bookDB :: BookDB } deriving (Show,Eq)
  
initialUserDB :: UserDB
initialUserDB = Map.empty

initialBookDB :: BookDB
initialBookDB = Map.empty

initialDB :: DataBase
initialDB = DataBase { userDB = initialUserDB, bookDB = initialBookDB }
