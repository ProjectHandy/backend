module Main where

import qualified Data.Map.Strict as Map

import Database
import Decode

register :: UserInfo -> UserDB -> UserDB
register (userName, emailAddress, password) =
  Map.insert userName (emailAddress, password, Map.empty)

postBookInfo :: UserInfo -> BookInfo -> UserDB -> UserDB
postBookInfo userInfo bookInfo userDB =
  let userInfo = (userName, _ , _) in
  Map.adjust (\(email,pwd,sellerInfo) -> (email,pwd, sellerInfo bookInfo)) userName userDB 

toNote :: String -> NotesTaken
toNote s =
  case s of
   "none"  -> None
   "few"   -> Few
   "heavy" -> Heavy

toPaper :: String -> PaperQuality
toPaper s =
  case s of
   "poor" -> Poor
   "average" -> Average
   "good" -> Good
   
getSellerInfo :: Map.Map String String -> SellerInfo
getSellerInfo dict =
  let bookName = fromJust $ Map.lookup "bookName" in
  let notes = toNote $ fromJust $ Map.lookup "notes" in
  let paper = toPaper $ fromJust $ Map.lookup "paper" in
  let price = read (fromJust $ Map.lookup "price") :: Float in
  Map.insert bookName (notes,paper,price) Map.empty
  
getBookInfo :: Map.Map String String -> BookInfo
getBookInfo dict =
  let isbn = fromJust $ Map.lookup "isbn" dict in
  let number = read (fromJust $ Map.lookup "number" dict) :: Int in
  let lowest = read (fromJust $ Map.lookup "lowest" dict) :: Float in
  let sellerInfo = getSellerInfo dict in
  BookInfo {isbn = isbn, number = number, lowest = lowest, sellerInfo = sellerInfo }

--the bookDB needs to be handled
update :: (String, Database) -> (String, Database)
update (s, database) =
  let (action, userInfo, dict) = parseInput s in
  let db = userDB database in
  case action of
    Register ->
      database { userDB = register userInfo db }
    PostBookInfo ->
      database { userDB = postBookInfo userInfo (getBookInfo dict) db }
