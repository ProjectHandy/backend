module Decode where

import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Map.Strict as Map

import qualified Database as D

data Action =
  -- basic action
    Register
  | Login
  -- seller action
  | PostBookInfo
  | GetProp
  | Propose
  -- buyer action
  | MatchBook
  | MatchClass
  | BuySearch 
    deriving (Show,Eq)

type ActionInfo = Map.Map String String

token :: String -> a -> ReadP a
token s a = const a <$> string s

parseAction :: ReadP Action
parseAction =
  token "register" Register
  <++ token "postbookinfo" PostBookInfo
  <++ token "login" Login
  <++ token "getprop" GetProp
  <++ token "matchbook" MatchBook
  <++ token "buysearch" BuySearch
  <++ token "propose" Propose

isSpecial c = elem c ['"', '\\']

notSpecial = not <$> isSpecial

parseStringlitInside :: ReadP Char
parseStringlitInside = satisfy notSpecial <++ (char '\\' *> satisfy (const True))

parseStringliteral :: ReadP String
parseStringliteral = between (satisfy (== '\"')) (satisfy (=='\"')) (many parseStringlitInside)

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
  
parseUserInfo :: ReadP D.UserInfo
parseUserInfo =
  parseOneField "\"user\"" >>= \username ->
  satisfy (==',') >>
  skipSpaces >>
  parseOneField "\"email\"" >>= \email ->
  satisfy (==',') >>
  skipSpaces >>
  parseOneField "\"pwd\"" >>= \password ->
  return $ D.UserInfo {D.user = username, D.email = email, D.pwd = password, D.token = ""}

parsePair :: ReadP (String, String)
parsePair =
  parseString >>= \key ->
  skipSpaces >>
  satisfy (== ':') >>
  skipSpaces >>
  parseString >>= \value ->
  return (key,value)

sep :: ReadP ()
sep = satisfy (== ',') >> skipSpaces

parsePairList :: ReadP [(String,String)]
parsePairList = sepBy parsePair sep

parseActionInfo :: ReadP ActionInfo
parseActionInfo = Map.fromList <$> parsePairList
  
parseInput :: ReadP (Action, ActionInfo)
parseInput =
  parseAction >>= \action ->
  satisfy (== '?') >>
  between (satisfy (== '{')) (satisfy (== '}')) parseActionInfo >>= \info ->
  return (action, info)

test = readP_to_S parseInput "register?{\"user\": \"cggong\", \"email\" : \"cggong@uchicago.edu\", \"pwd\": \"329fjmsdjsdmfsd\"}"

test1 = readP_to_S parseInput "matchbook?{indicator:iliad}"

test2 = readP_to_S parseInput "login?{\"user\": \"cggong\", \"email\" : \"cggong@uchicago.edu\", \"pwd\": \"23rujewfmis0&token=osfj0jf02imfeowfsd\"}"

test3 = readP_to_S parseInput "postbookinfo?{\"user\": \"cggong\", \"email\" : \"cggong@uchicago.edu\", \"pwd\": \"sfdinu9i323\", \"isbn\": \"9783249237\", \"notes\": 3, \"price\": 6.3, \"notesdesc\": \"Some notes taken, but acceptable :)\"}"

test4 = readP_to_S parseInput "propose?{\"id\": 1, \"buyer\": \"alice\", \"seller\": \"cggong\", \"buyerToSeller\" : \"true\", \"props\": \"[{date: 20160401, time: 15:00, place: \\\"Reg}]\"}"

test5 = readP_to_S parseInput "propose?{\"props\":\"[{\\\"time\\\":\\\"13:58\\\",\\\"date\\\":\\\"20001202\\\",\\\"place\\\":\\\"He \\\"}]\",\"pwd\":\"abc\",\"id\":\"0\",\"phone\":\"312314242\",\"buyer\":\"cggong@uchicago.edu\",\"user\":\"cggong\",\"email\":\"cggong@uchicago.edu\",\"seller\":\"cggong@uchicago.edu\",\"buyerToSeller\":\"true\"}"

-- postparsing check

allMember :: Ord k => Map.Map k a -> [k] -> Bool
allMember m xs = all (flip Map.member m) xs

userInfoCheck map = allMember map ["user", "email", "pwd"]
bookInfoCheck map = allMember map ["isbn", "notes", "paper", "price"]
proposeCheck map = allMember map ["id", "buyer", "phone", "props"]
getPropCheck map = allMember map ["email"]
matchBookCheck map = allMember map ["indicator"]
buySearchCheck map = allMember map ["isbn"]
matchClassCheck map = allMember map ["indicator"]

validPair :: (Action, ActionInfo) -> Bool
validPair (action, info) =
  case action of
     Register     -> userInfoCheck info
     Login        -> userInfoCheck info
     PostBookInfo -> bookInfoCheck info
     Propose      -> proposeCheck info
     GetProp      -> getPropCheck info
     MatchBook    -> matchBookCheck info
     MatchClass   -> matchClassCheck info
     BuySearch    -> buySearchCheck info
