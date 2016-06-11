module Decode where

import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Map.Strict as Map

import Database

data Action = Register
            | PostBookInfo
            | Login
            deriving (Show, Eq)

data Value = VString String | VFloat Float deriving (Show, Eq)

token :: String -> a -> ReadP a
token s a = const a <$> string s

parseAction :: ReadP Action
parseAction =
  token "register" Register
  <++ token "postbookinfo" PostBookInfo
  <++ token "login" Login

parseStringliteral :: ReadP String
parseStringliteral = between (satisfy (== '\"')) (satisfy (=='\"')) (munch1 (/= '\"'))

parseString :: ReadP String
parseString =
  let pred c = not (isSpace c) && c /= '}' && c /= '{' && c /= ',' && c /= ':' in
  parseStringliteral
  <++ (many1 $ satisfy pred)

parseOneField :: String -> ReadP String
parseOneField key =
  string key >>
  satisfy (== ':') >>
  skipSpaces >>
  parseString >>= \s ->
  return s
  
parseUserInfo :: ReadP UserInfo
parseUserInfo =
  parseOneField "\"user\"" >>= \username ->
  satisfy (==',') >>
  skipSpaces >>
  parseOneField "\"email\"" >>= \email ->
  satisfy (==',') >>
  skipSpaces >>
  parseOneField "\"pwd\"" >>= \password ->
  return (username, email, password)

parsePair :: ReadP (String, String)
parsePair =
  parseString >>= \key ->
  satisfy (== ':') >>
  skipSpaces >>
  parseString >>= \value ->
  return (key,value)

sep :: ReadP ()
sep = satisfy (== ',') >> skipSpaces

parsePairList :: ReadP [(String,String)]
parsePairList = sepBy parsePair sep

parseInput :: ReadP (Action, UserInfo, Map.Map String String)
parseInput =
  parseAction >>= \action ->
  satisfy (== '?') >>
  between (satisfy (== '{')) (satisfy (== '}'))
  (parseUserInfo >>= \userInfo ->
  optional sep >>
  parsePairList >>= \list ->
  return (action, userInfo, Map.fromList list))

test = readP_to_S parseInput "postbookinfo?{\"user\": \"cggong\", \"email\": cggong@uchicago.edu, \"pwd\": \"sfdinu9i323\", \"isbn\": \"9783249237\", \"notes\": 3, \"price\": 6.3, \"notesdesc\": \"Some notes taken, but acceptable :)\"}"
