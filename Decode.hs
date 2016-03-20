module Decode where

import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Map.Strict as Map

import Database

data Action = Register | PostBookInfo deriving (Show, Eq)

data Value = VString String | VFloat Float deriving (Show, Eq)

token :: String -> a -> ReadP a
token s a = const a <$> string s

bracket = between (satisfy (== '<')) (satisfy (== '>'))

parseAction :: ReadP Action
parseAction = bracket $
  token "register" Register
  <++ token "postBookInfo" PostBookInfo

bracketAndString :: String -> ReadP String
bracketAndString s = bracket $ string s

parseString :: ReadP String
parseString = bracket $ many1 $ satisfy isAlphaNum

parseUserInfo_ :: ReadP String
parseUserInfo_ = bracket $ many1 $ satisfy isAlphaNum

parseUserInfo :: ReadP UserInfo
parseUserInfo =
  bracketAndString "user" >>
  satisfy (== '=') >>
  parseUserInfo_ >>= \username ->
  satisfy (== '&') >>
  bracketAndString "pwd" >>
  satisfy (== '=') >>
  parseUserInfo_ >>= \password ->
  return $ (username, "", password)
{-
parseFloat :: ReadP Float
parseFloat = (\x -> read x :: Float) <$> many1 (satisfy (\x -> isDigit x || x == '.'))

parseValue :: ReadP Value
parseValue =
  (VFloat <$> parseFloat) <++ (VString <$> many1 (satisfy isAlphaNum))
-}

parsePair :: ReadP (String, String)
parsePair =
  parseString >>= \key ->
  satisfy (== '=') >>
  parseString >>= \value ->
  return (key,value)

parsePairList :: ReadP [(String,String)]
parsePairList = sepBy parsePair $ satisfy (== '&')

parseInput :: ReadP (Action, UserInfo, Map.Map String String)
parseInput =
  parseAction >>= \action ->
  satisfy (== '?') >>
  parseUserInfo >>= \userInfo ->
  optional (satisfy (== '&')) >>
  parsePairList >>= \list ->
  return (action, userInfo, Map.fromList list)

