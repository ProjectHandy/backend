module Decode where

import Text.ParserCombinators.ReadP
import Data.Char
import qualified Data.Map.Strict as Map

import Database

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
  | BuySearch
    deriving (Show,Eq)

data ActionInfo = Query String | Info (Map.Map String String) deriving (Show, Eq)

{-    
data SellAction =
  Register
  | PostBookInfo
  | Login
  | GetProp
  deriving (Show, Eq)
           
data BuyAction =
  MatchBook
  | BuySearch
  deriving (Show, Eq)-}
           
token :: String -> a -> ReadP a
token s a = const a <$> string s
{-
parseSellAction :: ReadP SellAction
parseSellAction =
  token "register" Register
  <++ token "postbookinfo" PostBookInfo
  <++ token "login" Login
  <++ token "getprop" GetProp

parseBuyAction :: ReadP BuyAction
parseBuyAction =
  token "matchbook" MatchBook
  <++ token "buysearch" BuySearch
  -}

parseAction :: ReadP Action
parseAction =
  token "register" Register
  <++ token "postbookinfo" PostBookInfo
  <++ token "login" Login
  <++ token "getprop" GetProp
  <++ token "matchbook" MatchBook
  <++ token "buysearch" BuySearch
  
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
{-
-- this is for the input that require user information
parseSellInput :: ReadP (SellAction, UserInfo, Map.Map String String)
parseSellInput =
  parseSellAction >>= \action ->
  satisfy (== '?') >>
  between (satisfy (== '{')) (satisfy (== '}'))
  (parseUserInfo >>= \userInfo ->
  optional sep >>
  parsePairList >>= \list ->
  return (action, userInfo, Map.fromList list))

-- this is for the buyer inquiry input
parseBuyInput :: ReadP (BuyAction, Query)
parseBuyInput =
  parseBuyAction >>= \action ->
  satisfy (== '?') >>
  between (satisfy (== '{')) (satisfy (== '}')) parseString >>= \s ->
  return (action, s)
-}

parseActionInfo :: ReadP ActionInfo
parseActionInfo =
  (Query <$> parseString)
  <++ (Info . Map.fromList <$> parsePairList)
  
parseInput :: ReadP (Action, ActionInfo)
parseInput =
  parseAction >>= \action ->
  satisfy (== '?') >>
  between (satisfy (== '{')) (satisfy (== '}')) parseActionInfo >>= \info ->
  return (action, info)

test = readP_to_S parseInput "postbookinfo?{\"user\": \"cggong\", \"email\": cggong@uchicago.edu, \"pwd\": \"sfdinu9i323\", \"isbn\": \"9783249237\", \"notes\": 3, \"price\": 6.3, \"notesdesc\": \"Some notes taken, but acceptable :)\"}"

test1 = readP_to_S parseInput "matchbook?{iliad}"

-- postparsing check

allMember :: Ord k => Map.Map k a -> [k] -> Bool
allMember m xs = all (flip Map.member m) xs

userInfoCheck (Query _) = False
userInfoCheck (Info map) = allMember map ["user", "email", "pwd"]

bookInfoCheck (Query _) = False
bookInfoCheck (Info map) = allMember map 
                           ["bookname", "isbn", "notes", "paper", "price"]

proposeCheck (Query _) = False
proposeCheck (Info map) = allMember map ["id", "buyer", "phone", "props"]
 
validPair :: (Action, ActionInfo) -> Bool
validPair (action, info) =
  case action of
     Register     -> userInfoCheck info
     Login        -> userInfoCheck info
     PostBookInfo -> bookInfoCheck info
     Propose      -> proposeCheck info
     _            -> True
