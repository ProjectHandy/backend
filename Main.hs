{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HMap
import System.Environment
import Data.Aeson

import Database
import Update

jsobj :: [(Text.Text, Value)] -> Value
jsobj = Object . HMap.fromList

jssobj :: [(Text.Text, Text.Text)] -> Value
jssobj = jsobj . fmap (fmap String)

jsslobj :: [(Text.Text, String)] -> Value
jsslobj = jssobj . fmap (fmap Text.pack)

                      
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
