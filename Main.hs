{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as Text
import Data.Maybe
import Text.ParserCombinators.ReadP
import System.Environment
import Data.Aeson
import Data.List
import qualified Data.ByteString.Lazy.Char8 as C

import Database
import qualified Decode as D

type Notification = (String, String)

register :: UserInfo -> UserDB -> UserDB
register userInfo =
  Map.insert (email userInfo) (userInfo, (Nothing, Map.empty), Map.empty)

extractPwd :: EMailAddress -> DataBase -> Password
extractPwd email db =
  let Just (userInfo,_,_) = Map.lookup email (userDB db) in pwd userInfo

postBookInfo :: UserInfo -> Map.Map String String -> DataBase -> Maybe (ID, DataBase)
postBookInfo userInfo dict database =
  let (email',pwd') = (email userInfo, pwd userInfo) in
  let genID = generateID database in
  let info = (\info -> info {bookid = genID}) <$> getBookInfo dict in
  let isbn = Map.lookup "isbn" dict in
  let title = Map.lookup "title" dict in
  let author = Map.lookup "author" dict in
  let pwd'' = extractPwd email' database in
  if pwd'' /= pwd' then Nothing else
  case (info, isbn, author, title) of
    (Just info', Just isbn', Just author', Just title') -> 
       let id = bookid info' in
       let sellerInfo = (info', Nothing) in
       let (userdb, bookdb) = (userDB database, bookDB database) in
       let foo (userInfo, (m,seller), buyer) = (userInfo, (m, Map.insert id sellerInfo seller), buyer) in    
       let new_userdb = Map.adjust foo email' userdb in
       let bar bookinfo =
               let curPrice = price info' in
               if curPrice > highest bookinfo then bookinfo {books = Map.insert genID (email',info') (books bookinfo), highest = curPrice}
               else if curPrice < lowest bookinfo then bookinfo {books = Map.insert genID (email',info') (books bookinfo), lowest = curPrice}
               else bookinfo {books = Map.insert genID (email',info') (books bookinfo)}
       in
       let new_bookdb =
             if Map.member isbn' bookdb then Map.adjust bar isbn' bookdb
             else let item = BookInfo {books = Map.singleton genID (email',info'), isbn = isbn', title = title', author = author', highest = price info', lowest = price info'} in Map.insert isbn' item bookdb
       in
       Just (genID, database { userDB = new_userdb, bookDB = new_bookdb })
    _ -> Nothing
    
generateID :: DataBase -> ID
generateID db =
  let bookdb = bookDB db in
  let currIDs = Map.foldr (\a acc -> Map.keys (books a) ++ acc) [] bookdb in
  let leastInteger xs = head $ dropWhile (flip elem xs) [0..]
  in
   leastInteger currIDs
   
getBookInfo :: Map.Map String String -> Maybe Info
getBookInfo dict =
  let notes = Map.lookup "notes" dict in
  let paper = Map.lookup "paper" dict in
  let price = Map.lookup "price" dict in
  let seller = Map.lookup "user" dict in
  let email = Map.lookup "email" dict in
  case (notes, paper, price, seller, email) of
    (Just notes, Just paper, Just price, Just seller, Just email) ->
      -- put dummy value for id here
      Just $ Info {bookid = -1,
                   note = read notes :: Int,
                   paper = read paper :: Int,
                   price = read price :: Float,
                   sellerName = seller,
                   sellerEmail = email}
    _                                             -> Nothing

getUserInfo :: Map.Map String String -> UserInfo
getUserInfo dict =
  let username = Map.lookup "user" dict in
  let email = Map.lookup "email" dict in
  let pwd = Map.lookup "pwd" dict in
  case (username, email, pwd) of
    (Just a, Just b, Just c) -> UserInfo {user = a,
                                          email = b,
                                          pwd = c,
                                          token = ""}
    _                        -> error "unable to get userInfo. Ill-formed dict."

-- this needs to be more efficient
removeBook :: ID -> DataBase -> DataBase
removeBook x db =
  let (userdb, bookdb) = (userDB db, bookDB db) in
  let bs = Map.filter (\b -> Map.member x $ books b) bookdb in
  case Map.toList bs of
    [b] -> let bookInfo = snd b in
           let newbookDict = Map.delete x $ books bookInfo in
           let newbookInfo = bookInfo {books = newbookDict} in
           let newBookdb = Map.adjust (const newbookInfo) (fst b) bookdb in 
              let modify (userInfo, sellerInfo, buyerInfo) =
                    let (t, dict) = sellerInfo in
                    let newSellerInfo = (t, Map.delete x dict)
                        newBuyerInfo  = Map.delete x buyerInfo
                    in
                    (userInfo, newSellerInfo, newBuyerInfo)
              in
              let sellers = Map.keys userdb in
              let newUserdb = foldr (Map.adjust modify) userdb sellers in
              db {bookDB = newBookdb, userDB = newUserdb}
    _      -> error $ "id " ++ show x ++ " is not unique!"

first (x,_,_) = x
second (_,x,_) = x
third (_,_,x) = x

update :: (String, DataBase) -> (String, DataBase, Maybe Notification)
update (s, database) =
  case readP_to_S D.parseInput s of
     []    -> ("{\"msg\":\"Error: parse error.\"}", database, Nothing)
     (x:_) ->
       let (action, dict) = fst x in
       if not $ D.validPair $ fst x then ("{\"msg\":\"Error: Not valid input.\"}", database, Nothing)
       else 
       let userdb = userDB database
           bookdb = bookDB database
       in
        case action of
         D.Register ->
           let userInfo = getUserInfo dict in
           let em = email userInfo in
           case Map.lookup em userdb of
             Just _  -> ("{\"msg\":\"Error: this email address has already been registered.\"}", database, Nothing)
             _       -> ("{\"msg\":\"register\"}", database { userDB = register userInfo userdb }, Nothing)
         D.PostBookInfo ->
           let userInfo = getUserInfo dict in
           case postBookInfo userInfo dict database of
             Nothing -> ("{\"msg\":\"Error: incorrect password\"}", database, Nothing)
             Just (id,db) -> ("{\"msg\":\"postbookinfo\",\"username\":"
                              ++ show (user userInfo)
                              ++ ",\"email\":"
                              ++ show (email userInfo)
                              ++ ",\"id\":" ++ show (show id) ++"}", db, Nothing)
         D.Login -> 
           let userInfo = getUserInfo dict in
           let Just token = Map.lookup "token" dict in
           let (em, pd) = (email userInfo, pwd userInfo) in
           case Map.lookup em userdb of
             Nothing -> ("{\"msg\":\"Error: unregistered user\"}", database, Nothing)
             Just (userinfo,_,_) -> if pd == pwd userinfo
                                    then
                                      let modify (userInfo, sInfo, bInfo) = (userInfo {token = token}, sInfo, bInfo) in
                                      let newUserdb = Map.adjust modify em userdb in
                                      ("{\"msg\":\"login\"}", database {userDB = newUserdb}, Nothing) 
                                    else ("{\"msg\":\"Error: incorrect password\"}", database, Nothing)
         D.Propose -> 
           let Just id = read <$> Map.lookup "id" dict in
           let Just prop = Map.lookup "props" dict in
           let Just decode_prop = decode (C.pack prop) :: Maybe [PropInfo] in
           let Just seller = Map.lookup "seller" dict in
           let Just buyer = Map.lookup "buyer" dict in
           let Just flag = (\x -> if x == "true" then True else False) <$> Map.lookup "buyerToSeller" dict in
           let chat = Map.lookup "chat" dict in
           if flag then 
           let userInfo = first <$> Map.lookup seller userdb in
           let sellerInfo = second <$> Map.lookup seller userdb in
           let tradeInfo = (snd <$> sellerInfo) >>= (Map.lookup id) in
           case (sellerInfo, tradeInfo, userInfo) of
             (Just sellerInfo, Just tradeInfo, Just userInfo) -> 
                case snd tradeInfo of
                   Nothing -> let bookinfo = fst tradeInfo in
                              let t = (bookinfo, Just $ Prop {Database.id = id, buyer = buyer, seller = seller, buyerToSeller = flag, chat = chat, propInfo = decode_prop}) in
                              let modify (userInfo, (_,s), b) = (userInfo, (Just t,s), b) in
                              let new_userdb = Map.adjust modify seller userdb in
                              let notification = Just (token userInfo, "Please choose a time for meeting.") in
                              ("{\"msg\":\"please choose a time for meeting\"}", database {userDB = new_userdb}, notification)
                   Just prop ->
                       case propInfo prop `intersect` decode_prop of
                           [] -> let (bookinfo, _) = tradeInfo in 
                                 let t = (bookinfo, Just $ Prop {Database.id = id, buyer = buyer, seller = seller, buyerToSeller = flag, chat = chat, propInfo = decode_prop}) in
                                 let modify (userInfo, (_,s), b) = (userInfo, (Just t,s), b) in
                                 let new_userdb = Map.adjust modify seller userdb in
                                 let notification = Just (token userInfo, "Please choose a time for meeting.") in
                                 ("{\"msg\":\"please choose a time for meeting\"}", database {userDB = new_userdb}, notification)
                           (x:_) -> ("{\"msg\": meeting time is" ++ show x ++ "}", removeBook id database, Nothing)
             (Nothing, _,_) -> ("{\"msg\": \"Error: seller does not exist!\"}", database, Nothing)
             (_, Nothing,_) -> ("{\"msg\": \"Error: id does not exist!\"}", database, Nothing)
           else 
              case first <$> Map.lookup buyer userdb of
                Nothing -> ("{\"msg\":\"buyer does not exist!\"}", database, Nothing)
                Just userInfo -> 
                      let notification = Just (token userInfo, "buyer " ++ buyer ++ " please respond.") in
                      ("{\"msg\": buyer " ++ buyer ++ " please respond}", database, notification)
         D.BuySearch ->
           let Just isbn = Map.lookup "isbn" dict in
           case Map.lookup isbn bookdb of
             Nothing -> ("{\"msg\":\"Error : cannot find the required book\"}", database, Nothing)
             Just b -> let items = map snd $ Map.elems $ books b in
                       let s = "{\"msg\":\"buysearch\",\"items\":" ++ (show $ C.unpack $ encode items) ++ "}" in 
                       (s , database, Nothing)
         D.MatchBook -> 
           let Just name = Map.lookup "indicator" dict in
           let bookDict = Map.filter (\x -> title x == name) bookdb in
           case Map.null bookDict of
              True -> ("{\"msg\":\"Error: cannot find any book with title " ++ name ++ "\"}", database, Nothing)
              _    -> ("{\"msg\":\"matchbook\",\"items\":" ++ show (C.unpack $ encode $ Map.elems bookDict) ++ "}", database, Nothing)
         D.GetProp ->
           let Just email = Map.lookup "email" dict in
           case Map.lookup email userdb of
              Nothing -> ("{\"msg\":\"Error: unregistered user\"}", database, Nothing)
              Just userInfo -> 
                 let (t, dict) = second userInfo in
                 case t of
                   Nothing -> ("{\"msg\":\"getprop\"}", database, Nothing)
                   Just t -> let (info, prop) = t in
                             let new_dict = Map.insert (bookid info) t dict in
                             let new_userdb = Map.adjust (\(userInfo,_,buyerInfo) -> (userInfo,(Nothing,new_dict),buyerInfo)) email userdb in
                             let new_db = database {userDB = new_userdb} in
                             let str = C.unpack $ encode prop in
                             let s = "{\"msg\":\"getprop\"" ++ tail str in
                             (s,new_db, Just (token $ first userInfo, s))
                                    


main :: IO ()
main = do
     let file = "db"
     dbString <- readFile file :: IO String
     let db = read dbString :: DataBase
     request <- head <$> getArgs :: IO String
     let (reply, newdb, maybeNotif) = update (request, db)
     let notifObj = case maybeNotif of
           Just (token, contents) -> jsslobj [("token", token), ("contents", contents)]
           _ -> Bool False
     let replyObj = jsobj [("reply", String (Text.pack reply)), ("notif", notifObj)]
     C.putStrLn (encode replyObj) :: IO ()
     length dbString `seq` writeFile file (show newdb :: String)
     return ()


updateList :: [String] -> DataBase -> ([String], DataBase)
updateList xs db =
  case xs of
    [] -> ([], db)
    (x:xs') -> let (s, newdb, _) = update (x,db) in
               let (ys, db') = updateList xs' newdb in
               (s:ys, db')
{-               
main :: IO ()
main = do
  let inFile = "test_input"
  let outFile = "test_output"
  testString <- readFile inFile
  let inputStr = lines testString
  let (outputStr, newdb) = updateList inputStr initialDB
  writeFile outFile $ unlines outputStr
-}

test = decode "{\"foo\": 123}" :: Maybe Value

test2 = Just (Object (HMap.fromList [("foo",Number 123.0)]))

jsobj :: [(Text.Text, Value)] -> Value
jsobj = Object . HMap.fromList

jssobj :: [(Text.Text, Text.Text)] -> Value
jssobj = jsobj . fmap (fmap String)

jsslobj :: [(Text.Text, String)] -> Value
jsslobj = jssobj . fmap (fmap Text.pack)

test3 = jssobj [("1","2")]
