module Test where

import Text.ParserCombinators.ReadP

import Database
import Decode
import Main

import qualified Data.Vector as V
import Data.Csv
import qualified Data.ByteString.Lazy.Char8 as C

db0 = initialDB
inputReg0 = "register?{\"email\":\"cggong@uchicago.edu\",\"user\":\"cggong\",\"pwd\":\"abc\"}"
result1 = update (inputReg0, db0)
(ret0, db1, _) = result1
-- ret0 == "{\"msg\":\"register\"}"
-- db1 == DataBase {userDB = fromList [("cggong@uchicago.edu",(UserInfo {user = "cggong", email = "cggong@uchicago.edu", pwd = "abc", token = ""},(Nothing,fromList []),fromList []))], bookDB = fromList []}

inputPost1 = "postbookinfo?{\"notes\":\"5\",\"isbn\":\"123\",\"pwd\":\"abc\",\"price\":\"3.5\",\"paperdesc\":\"good\",\"paper\":\"3\",\"pricedesc\":\"nothing\",\"user\":\"cggong\",\"email\":\"cggong@uchicago.edu\",\"title\":\"Iliad\",\"author\":\"Homer\"}"
result2 = update (inputPost1, db1)
(ret1, db2, _) = result2
-- ret1 == "{\"msg\":\"postbookinfo\", \"id\":\"0\"}"
-- db2 == DataBase {userDB = fromList [("cggong@uchicago.edu",(UserInfo {user = "cggong", email = "cggong@uchicago.edu", pwd = "abc", token = ""},(Nothing,fromList [(0,(Info {bookid = 0, note = 5, paper = 3, price = 3.5},Nothing))]),fromList []))], bookDB = fromList []}

inputMBook2 = "matchbook?{\"user\":\"cggong\",\"pwd\":\"abc\",\"email\":\"cggong@uchicago.edu\",\"indicator\":\"Iliad\"}"
result3 = update (inputMBook2, db2)
(ret2, db3, _) = result3

str = "[{\"time\":\"21:21\",\"date\":\"20001202\",\"place\":\"Good\"}]"
inputProp3 = "propose?{\"props\":\"[{\\\"time\\\":\\\"21:21\\\",\\\"date\\\":\\\"20001202\\\",\\\"place\\\":\\\"Good\\\"}]\",\"pwd\":\"abc\",\"id\":\"0\",\"phone\":\"312314242\",\"buyer\":\"cggong\",\"email\":\"cggong@uchicago.edu\",\"seller\":\"cggong@uchicago.edu\",\"buyerToSeller\":\"true\"}"
result4 = update (inputProp3, db3)
(ret3,db4,notif) = result4

getClassInfo = do
    classdb <- readFile "test_class_db.csv"
    let output = decode HasHeader (C.pack classdb) :: Either String (V.Vector [String])
    case output of
      Left s -> error s
      Right vecs -> return vecs
