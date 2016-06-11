module Database where

import qualified Data.Map.Strict as Map

type UserName = String
type EMailAddress = String
type Password = String

data NotesTaken = Notes Int deriving (Show, Eq)
data PaperQuality = Paper Int deriving (Show, Eq)
type Price = Float

type BookName = String -- bookName refers to the ISBN 

type Info = (NotesTaken, PaperQuality, Price)
type SellInfo = Map.Map UserName Info --for bookDB
type SellerInfo = Map.Map BookName Info -- for UserDB

type UserInfo = (UserName, EMailAddress, Password)

type UserDB = Map.Map EMailAddress (UserName, Password, SellerInfo)

data BookInfo =
  BookInfo { title :: String, number :: Int, lowest :: Float, sellInfo :: SellInfo } deriving (Show, Eq) 

type BookDB = Map.Map BookName BookInfo

data DataBase =
  DataBase { userDB :: UserDB, bookDB :: BookDB } deriving (Show,Eq)
  
initialUserDB :: UserDB
initialUserDB = Map.empty

initialBookDB :: BookDB
initialBookDB = Map.empty

initialDB :: DataBase
initialDB = DataBase { userDB = initialUserDB, bookDB = initialBookDB }
