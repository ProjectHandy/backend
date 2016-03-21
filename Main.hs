module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP

import Database
import Decode

register :: UserInfo -> UserDB -> UserDB
register (userName, emailAddress, password) =
  Map.insert userName (emailAddress, password, Map.empty)

postBookInfo :: UserInfo -> Map.Map String String -> DataBase -> Maybe DataBase
postBookInfo userInfo dict database =
  let (userName, _ , _) = userInfo in
  let info = getInfo dict in
  let sellerinfo = getSellerInfo dict in
  case (info, sellerinfo) of
    (Just info', Just sellerinfo') -> 
       let (userdb, bookdb) = (userDB database, bookDB database) in
       let foo (email, pwd, sellerDict) = (email,pwd, Map.insert (fst sellerinfo') (snd sellerinfo') sellerDict) in    
       let new_userdb = Map.adjust foo userName userdb in
       let bar bookinfo = bookinfo { sellInfo = Map.insert userName info' (sellInfo bookinfo) } in
       let new_bookdb = Map.adjust bar (fst sellerinfo') bookdb in
       Just $ database { userDB = new_userdb, bookDB = new_bookdb }
    _ -> Nothing

toNote :: String -> NotesTaken
toNote s = Notes (read s :: Int)

toPaper :: String -> PaperQuality
toPaper s = Paper (read s :: Int)
   
getInfo :: Map.Map String String -> Maybe Info
getInfo dict =
  --let title = Map.lookup "title" dict in
  let notes = Map.lookup "notes" dict in
  let paper = Map.lookup "paper" dict in
  let price = Map.lookup "price" dict in
  case (notes, paper, price) of
    (Just a, Just b, Just c) -> Just $ (toNote a,toPaper b,read c :: Float)
    _                        -> Nothing
  
getSellerInfo :: Map.Map String String -> Maybe (BookName, Info)
getSellerInfo dict =
  let bookname = Map.lookup "isbn" dict in
  let info = getInfo dict in
  case (bookname, info) of
    (Just bookname', Just info') -> Just (bookname', info')
    _                            -> Nothing
    

-- TODO: notification
update :: (String, DataBase) -> (String, DataBase)
update (s, database) =
  case readP_to_S parseInput s of
     []    -> ("{msg: parse error}", database)
     (x:_) ->
       let (action, userInfo, dict) = fst x in
       let db = userDB database in
        case action of
         Register ->
           let (username, _, _) = userInfo in
           case Map.lookup username db of
             Just _  -> ("{msg: this name is already registered}", database)
             _       -> ("msg", database { userDB = register userInfo db })
         PostBookInfo ->
           case postBookInfo userInfo dict database of
             Nothing -> ("{msg: Error}", database)
             Just db -> ("msg", db)
         Login -> 
           let (username, _, pwd) = userInfo in
           case Map.lookup username db of
             Nothing -> ("{msg: unregistered user}", database)
             Just userinfo -> let (_, pwd',_) = userinfo in
                              if pwd == pwd' then ("msg", database) 
                              else ("{msg: incorrect password}", database)
