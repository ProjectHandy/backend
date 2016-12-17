{-# LANGUAGE OverloadedStrings #-}

module Update where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP
import System.Environment
import Data.Aeson
import Data.List
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C

import Database
import qualified Decode as D

type Notification = (String, String)

register :: UserInfo -> UserDB -> UserDB
register userInfo =
  Map.insert (email userInfo) (userInfo, ([], Map.empty), ([], Map.empty))

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
       let foo (userInfo, (l, dict), buyer) = (userInfo, (l, Map.insert id sellerInfo dict), buyer) in    
       let new_userdb = Map.adjust foo email' userdb in
       let bar bookinfo =
               let curPrice = price info' in
               if curPrice > highest bookinfo then bookinfo {books = Map.insert genID (email',info') (books bookinfo), highest = curPrice}
               else if curPrice < lowest bookinfo then bookinfo {books = Map.insert genID (email',info') (books bookinfo), lowest = curPrice}
               else bookinfo {books = Map.insert genID (email',info') (books bookinfo)}
       in
       let new_bookdb =
             if Map.member isbn' bookdb then Map.adjust bar isbn' bookdb
             else let item = BookInfo {
                        books = Map.singleton genID (email',info'),
                        isbn = isbn',
                        title = title',
                        author = author',
                        highest = price info',
                        lowest = price info'
                        }
                  in
                    Map.insert isbn' item bookdb
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
      Just $ Info {
      bookid = -1,
      note = read notes :: Int,
      paper = read paper :: Int,
      price = read price :: Float,
      sellerName = seller,
      sellerEmail = email,
      removed = False
      }
    _ -> Nothing

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
           let newBooks = Map.adjust (\(e, info) -> (e, info {removed = True})) x (books bookInfo) in
           let newbookInfo = bookInfo {books = newBooks} in
           let newBookdb = Map.adjust (const newbookInfo) (fst b) bookdb in 
           let modify (userInfo, sellerInfo, buyerInfo) =
                 let modifyTradeInfo x info = Map.adjust (\(info, p) -> (info {removed = True}, p)) x info in
                 let (l1, dict1) = sellerInfo
                     (l2, dict2) = buyerInfo
                 in
                   let newSellerInfo = (l1, modifyTradeInfo x dict1)
                       newBuyerInfo  = (l2, modifyTradeInfo x dict2)
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

update :: (String, DataBase, ClassDB) -> (String, DataBase, Maybe Notification)
update (s, database, classdb) =
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
           let Just flag = (\x -> x == "true") <$> Map.lookup "buyerToSeller" dict in
           let chat = Map.lookup "chat" dict in
           if flag then 
           let sellerUserInfo = first <$> Map.lookup seller userdb
               buyerUserInfo = first <$> Map.lookup buyer userdb
           in
           let sellerInfo = second <$> Map.lookup seller userdb in
           let tradeInfo = (snd <$> sellerInfo) >>= (Map.lookup id) in
           case (sellerInfo, tradeInfo, sellerUserInfo, buyerUserInfo) of
             (Just sellerInfo, Just tradeInfo, Just userInfo, Just bUserInfo) -> 
                case snd tradeInfo of
                   -- if Maybe Prop is Nothing, then the book hasn't been requested,
                   -- so we remove the requested book here and update buyer info
                   Nothing -> let bookinfo = fst tradeInfo in
                              let t = (bookinfo, Just $ Prop {Database.id = id, buyer = buyer, seller = seller, buyerToSeller = flag, chat = chat, propInfo = decode_prop}) in
                              let modifySeller (userInfo, (l,s), b) = (userInfo, (t:l,s), b)
                                  modifyBuyer (userInfo, s, (l,b)) = (userInfo, s, (l, Map.insert id (bookinfo, Nothing) b))
                              in
                              let new_userdb = Map.adjust modifyBuyer buyer $ Map.adjust modifySeller seller userdb in
                              let newdb = removeBook id (database {userDB = new_userdb}) in
                              let notification = case (decode_prop, chat) of
                                    ([], Just chat) -> Just (token userInfo, user bUserInfo ++ ": " ++ chat)
                                    _            -> Just (token userInfo, "Please choose a time for meeting.")
                              in
                              ("{\"msg\":\"propose\"}", newdb, notification)
                   Just prop ->
                       case propInfo prop `intersect` decode_prop of
                           [] -> let (bookinfo, _) = tradeInfo in 
                                 let t = (bookinfo, Just $ Prop {Database.id = id, buyer = buyer, seller = seller, buyerToSeller = flag, chat = chat, propInfo = decode_prop}) in
                                 let modify (userInfo, (l,s), b) = (userInfo, (t:l,s), b) in
                                 let new_userdb = Map.adjust modify seller userdb in
                                 let notification = case (decode_prop, chat) of
                                       ([], Just chat) -> Just (token userInfo, user bUserInfo ++ ": " ++ chat)
                                       _            -> Just (token userInfo, "Please choose a time for meeting.")
                                 in
                                 ("{\"msg\":\"propose\"}", database {userDB = new_userdb}, notification)
                           -- if there is intersection, the proposal must be an actual proposal, not a chat.
                           (x:_) -> let notification = Just (token userInfo, "meeting time is " ++ show x) in 
                                    ("{\"msg\": \"propose\"}", database, notification)
             (Nothing, _, _, _) -> ("{\"msg\": \"Error: seller does not exist!\"}", database, Nothing)
             (_, Nothing, _, _) -> ("{\"msg\": \"Error: id does not exist!\"}", database, Nothing)
             (_, _, _, Nothing) -> ("{\"msg\": \"Error: buyer does not exist!\"}", database, Nothing)
           else
             let sellerUserInfo = first <$> Map.lookup seller userdb
                 buyerUserInfo  = first <$> Map.lookup buyer userdb
                 buyerInfo = second <$> Map.lookup buyer userdb
                 tradeInfo = (snd <$> buyerInfo) >>= (Map.lookup id)
             in
              case (sellerUserInfo, buyerUserInfo) of
                (Nothing, _) -> ("{\"msg\":\"Error: seller does not exist!\"}", database, Nothing)
                (_, Nothing) -> ("{\"msg\":\"Error: buyer does not exist!\"}", database, Nothing)
                (Just sUserInfo, Just bUserInfo) ->
                      let Just bInfo = buyerInfo
                          Just tInfo = tradeInfo
                      in
                      let bookinfo = fst tInfo in
                      let t = (bookinfo, Just $ Prop {Database.id = id, buyer = buyer, seller = seller, buyerToSeller = flag, chat = chat, propInfo = decode_prop}) in
                      let modify (userInfo, (l,s), b) = (userInfo, (t:l,s), b) in
                      let new_userdb = Map.adjust modify buyer userdb in
                      let newdb = database {userDB = new_userdb} in
                      let notification = case (decode_prop, chat) of
                           ([], Just chat) -> Just (token bUserInfo, user sUserInfo ++ ": " ++ chat)
                           _            ->  Just (token bUserInfo, "buyer " ++ buyer ++ " please respond.")
                      in
                      ("{\"msg\":\"propose\"}", newdb, notification)
         D.BuySearch ->
           let Just isbn = Map.lookup "isbn" dict in
           case Map.lookup isbn bookdb of
             Nothing -> ("{\"msg\":\"Error : cannot find the required book\"}", database, Nothing)
             Just b ->
               if bookSize b == 0 then
                 ("{\"msg\":\"Error : cannot find the required book\"}", database, Nothing)
               else
               let items = bookToInfo b in
                 let s = "{\"msg\":\"buysearch\",\"items\":" ++ (show $ C.unpack $ encode items) ++ "}" in 
                   (s, database, Nothing)
         D.MatchBook -> 
           let Just name = Map.lookup "indicator" dict in
           -- search for books of a class section
           if '/' `elem` name then
           let Just splitIndex = elemIndex '/' name in
           let (classNumber, section_) = splitAt splitIndex name in
           let section = tail section_ in
           case Map.lookup (classNumber, section) classdb of
             Nothing -> ("{\"msg\": \"Error: cannot find class " ++ name ++ "\"}", database, Nothing)
             Just classinfo -> 
                   let bookInfoList = map (fromJust . flip Map.lookup bookdb) (bookID classinfo) in 
                   ("{\"msg\":\"matchbook\", \"items\":" ++ (show $ C.unpack $ encode bookInfoList) ++ "}", database, Nothing)
           -- search for isbn
           else if all isDigit name then
           case Map.lookup name bookdb of
             Nothing -> ("{\"msg\": \"Error: cannot find the required book\"}", database, Nothing)
             Just b  -> if bookSize b == 0
               then ("{\"msg\": \"Error: cannot find the required book\"}", database, Nothing)
               else ("{\"msg\":\"matchbook\",\"items\":" ++ 
                 show (C.unpack $ encode $ [b]) ++ "}", 
                 database, Nothing)
           -- search for name
           else
           let pred x = isInfixOf (map toLower name) (map toLower $ title x) in
           let bookDict = Map.filter pred bookdb in
           case Map.null bookDict of
              True -> ("{\"msg\":\"Error: cannot find any book with title " ++ name ++ "\"}", database, Nothing)
              _    -> ("{\"msg\":\"matchbook\",\"items\":" ++ show (C.unpack $ encode $ Map.elems bookDict) ++ "}", database, Nothing)
           
         D.GetProp ->
           let Just email = Map.lookup "email" dict in
           case Map.lookup email userdb of
              Nothing -> ("{\"msg\":\"Error: unregistered user\"}", database, Nothing)
              Just userInfo -> 
                 let (l1, dict1) = second userInfo
                     (l2, dict2) = third userInfo
                 in
                 let modifyTradeInfo flag t l =
                       let (info, prop) = t
                           dict = if flag then dict1 else dict2
                       in
                       let new_dict = Map.insert (bookid info) t dict in
                       let new_userdb = if flag
                             then Map.adjust
                                 (\(userInfo, _, buyerInfo) -> (userInfo, (l, new_dict), buyerInfo))
                                 email userdb
                             else Map.adjust
                                 (\(userInfo, sellerInfo, _) -> (userInfo, sellerInfo, (l,new_dict)))
                                 email userdb
                       in
                       let new_db = database {userDB = new_userdb} in
                       let str = C.unpack $ encode prop in
                       let s = "{\"msg\":\"getprop\"," ++ tail str in
                         (s,new_db, Nothing)
                 in
                 case (l1, l2) of
                   ([], []) -> ("{\"msg\":\"getprop\"}", database, Nothing)
                   (t:l, _) -> modifyTradeInfo True t l
                   (_, t:l) -> modifyTradeInfo False t l
         D.MatchClass -> 
            let Just name = Map.lookup "indicator" dict in
            let classList = Map.toList $ Map.filterWithKey 
                                         (\(cl, sect) _ -> cl == name) classdb in
            ("{\"msg\":\"matchClass\",\"classes\":" ++ show (C.unpack $ encode classList) ++ "}", database, Nothing)

{-
getClassInfo :: IO ClassDB
getClassInfo =
             readFile "test_class_db.csv" >>= \classdb ->
             let output = Csv.decode Csv.HasHeader (C.pack classdb) :: Either String (V.Vector [String]) in
             case output of 
               Left s -> error s
               Right vec -> return $ V.foldr combine Map.empty vec


main :: IO ()
main = do
     let file = "db"
     dbString <- readFile file :: IO String
     let db = read dbString :: DataBase
     request <- head <$> getArgs :: IO String
     classdb <- getClassInfo
     let (reply, newdb, maybeNotif) = update (request, db, classdb)
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
          
main :: IO ()
main = do
  let inFile = "test_input"
  let outFile = "test_output"
  testString <- readFile inFile
  let inputStr = lines testString
  let (outputStr, newdb) = updateList inputStr initialDB
  writeFile outFile $ unlines outputStr
-}
