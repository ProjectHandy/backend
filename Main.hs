{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP
import System.Environment
import Data.Aeson
import Data.List
import qualified Data.ByteString.Lazy.Char8 as C

import Database
import Decode

register :: UserInfo -> UserDB -> UserDB
register (userName, email, password) =
  Map.insert email (userName, password, Map.empty, Map.empty)

extractPwd :: EMailAddress -> DataBase -> Password
extractPwd email db =
  let Just (_,pwd,_,_) = Map.lookup email (userDB db) in pwd

postBookInfo :: UserInfo -> Map.Map String String -> DataBase -> Maybe DataBase
postBookInfo userInfo dict database =
  let (_, email,pwd) = userInfo in
  let genID = generateID database in
  let info = (\(_,b,c,d) -> (genID, b,c,d)) <$> getBookInfo dict in
  let isbn = Map.lookup "isbn" dict in
  let pwd' = extractPwd email database in
  if pwd' /= pwd then Nothing else
  case (info, isbn) of
    (Just info', Just isbn') -> 
       let id = first info' in
       let sellerInfo = (info', []) in
       let (userdb, bookdb) = (userDB database, bookDB database) in
       let foo (username, pwd, seller, buyer) = (username, pwd, Map.insert id sellerInfo seller, buyer) in    
       let new_userdb = Map.adjust foo email userdb in
       let bar bookinfo = bookinfo {sellInfo = Map.insert email info' (sellInfo bookinfo)} in
       let new_bookdb = Map.adjust bar isbn' bookdb in
       Just $ database { userDB = new_userdb, bookDB = new_bookdb }
    _ -> Nothing

toNote :: String -> NotesTaken
toNote s = Notes (read s :: Int)

toPaper :: String -> PaperQuality
toPaper s = Paper (read s :: Int)

generateID :: DataBase -> ID
generateID db =
  let bookdb = bookDB db in
  let currIDs = map identifier $ Map.elems bookdb in
  let leastInteger xs = head $ dropWhile (flip elem xs) [0..]
  in
   leastInteger currIDs
   
getBookInfo :: Map.Map String String -> Maybe Info
getBookInfo dict =
  let notes = Map.lookup "notes" dict in
  let paper = Map.lookup "paper" dict in
  let price = Map.lookup "price" dict in
  case (notes, paper, price) of
    (Just notes, Just paper, Just price) ->
      -- put dummy value for id here
      Just $ (-1, toNote notes,toPaper paper,read price :: Float)
    _                                             -> Nothing

getUserInfo :: Map.Map String String -> UserInfo
getUserInfo dict =
  let username = Map.lookup "user" dict in
  let email = Map.lookup "email" dict in
  let pwd = Map.lookup "pwd" dict in
  case (username, email, pwd) of
    (Just a, Just b, Just c) -> (a,b,c)

first (a,_,_,_) = a
second (_,b,_,_) = b
third (_,_,c,_) = c
fourth (_,_,_,d) = d

removeBook :: ID -> DataBase -> DataBase
removeBook x db =
  let (userdb, bookdb) = (userDB db, bookDB db) in
  let newBookdb = Map.filter (((==) x) . identifier) bookdb in
  case Map.toList newBookdb of
    [book] -> let sellers = Map.keys $ sellInfo (snd book) in
              let modify (username, pwd, sellerInfo, buyerInfo) =
                    let newSellerInfo = Map.delete x sellerInfo
                        newBuyerInfo  = Map.delete x buyerInfo
                    in
                    (username, pwd, newSellerInfo, newBuyerInfo)
              in
              let newUserdb = foldr (Map.adjust modify) userdb sellers in
              db {bookDB = newBookdb, userDB = newUserdb}
    _      -> error $ "id " ++ show x ++ " is not unique!"

-- TODO: notification
update :: (String, DataBase) -> (String, DataBase)
update (s, database) =
  case readP_to_S parseInput s of
     []    -> ("{\"msg\":\"Error: parse error.\"}", database)
     (x:_) ->
       let (action, info) = fst x in
       if not $ validPair $ fst x then ("{\"msg\":\"Error: Not valid input.\"}", database)
       else 
       let userdb = userDB database
           bookdb = bookDB database
       in
        case action of
         Register ->
           let Info dict = info in
           let userInfo = getUserInfo dict in
           let (_, email, _) = userInfo in
           case Map.lookup email userdb of
             Just _  -> ("{\"msg\":\"Error: this email address has already been registered.\"}", database)
             _       -> ("{\"msg\":\"register\"}", database { userDB = register userInfo userdb })
         PostBookInfo ->
           let Info dict = info in
           let userInfo = getUserInfo dict in
           case postBookInfo userInfo dict database of
             Nothing -> ("{\"msg\":\"Error: incorrect password\"}", database)
             Just db -> ("{\"msg\":\"postbookinfo\"}", db)
         Login -> 
           let Info dict = info in
           let userInfo = getUserInfo dict in
           let (_, email, pwd) = userInfo in
           case Map.lookup email userdb of
             Nothing -> ("{\"msg\":\"Error: unregistered user\"}", database)
             Just userinfo -> let (_,pwd',_,_) = userinfo in
                              if pwd == pwd' then ("msg", database) 
                              else ("{\"msg\":\"Error: incorrect password\"}", database)
         Propose -> 
           let (userdb, bookdb) = (userDB database, bookDB database) in
           let Info dict = info in 
           let Just id = read <$> Map.lookup "id" dict in
           let Just prop = Map.lookup "prop" dict in
           let Just decode_prop = decode (C.pack prop) :: Maybe [PropInfo] in
           let Just seller = Map.lookup "seller" dict in
           let Just buyer = Map.lookup "buyer" dict in
           let Just flag = (\x -> if x == "true" then True else False) <$> Map.lookup "buyerToSeller" dict in
           let chat = Map.lookup "chat" dict in
           if flag then 
           let sellerInfo = third <$> Map.lookup seller userdb in
           let tradeInfo = sellerInfo >>= (Map.lookup id) in
           case (sellerInfo, tradeInfo) of
             (Just sellerInfo, Just tradeInfo) -> 
                let propInfo' = map propInfo (snd tradeInfo) in
                case propInfo' `intersect` decode_prop of
                    [] -> ("{\"msg\":\"please choose a time for meeting\"}", database)
                    (x:_) -> ("{\"msg\": meeting time is" ++ show x ++ "}", removeBook id database)
             (Nothing, _) -> ("{\"msg\": \"Error: seller does not exist!\"}", database)
             (_, Nothing) -> ("{\"msg\": \"Error: id does not exist!\"}", database)
           else ("{\"msg\": buyer " ++ buyer ++ " please respond}", database)
         -- need to convert to return string to json format
         MatchBook ->
           let Query s = info in
           let books = Map.toList $ Map.filter (\x -> title x == s) bookdb in
           if books == [] then ("{\"msg\":\"Error : cannot find the required book\"}", database)
           else ("books:" ++ foldr ((++) . show) "" books, database)
         BuySearch -> 
           let Query s = info in
           case Map.lookup s bookdb of
              Nothing -> ("{\"msg\": \"Error: cannot find the required book\"" ++ s ++ "}", database)
              Just x -> ("{\"msg\":\"buysearch\", \"book\": isbn: " ++ s ++ show x ++"}", database) 

{-
main :: IO ()
main = do
     let file = "db"
     dbString <- readFile file :: IO String
     let db = read dbString :: DataBase
     request <- head <$> getArgs :: IO String
     let (reply, newdb) = update (request, db)
     putStrLn reply :: IO ()
     length dbString `seq` writeFile file (show newdb :: String)
     return ()
-}

updateList :: [String] -> DataBase -> ([String], DataBase)
updateList xs db =
  case xs of
    [] -> ([], db)
    (x:xs') -> let (s, newdb) = update (x, db) in
               let (ys, db') = updateList xs' newdb in
               (s:ys, db')
               
main :: IO ()
main = do
  let inFile = "test_input"
  let outFile = "test_output"
  testString <- readFile inFile
  let inputStr = lines testString
  let (outputStr, newdb) = updateList inputStr initialDB
  writeFile outFile $ unlines outputStr
