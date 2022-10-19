module Task2 where

import Task2Message1

import Data.Either(either)
import Data.List as L 
import Data.Char as C 
import Data.Function

--data JsonLikeValue = JLString String | JLInt Int | JLMap [(String, JsonLikeValue)] | JLArray [JsonLikeValue] deriving (Show, Eq)

--parse :: Int                         -- ^ Size of the matrix (number of columns or rows)
--     -> String                      -- ^ Encoded message
--     -> Either String JsonLikeValue -- ^ Parsed data structure or error message
checkParse :: Bool
checkParse =
    case (parse size message, expectedParse) of
    (Right m, Right e) -> m == e
    (Left _, Left _) -> True
    _ -> False

checkConvert :: Bool
checkConvert = convert size (either error id (parse size message')) == expectedConvert


parse :: Int -> String -> Either String JsonLikeValue
parse _ str = 
    case parse' str of
        Right (j, _) -> Right j
        Left error -> Left error

parse' :: String -> Either String (JsonLikeValue, String)
parse' ('d':r) =
    case parseListOfMoves r [] of
        Right (j, s) -> Right (j, s)
        Left error -> Left error
parse' _ = Left "Not a correct move set"

parseListOfMoves :: String -> [(String, JsonLikeValue)] -> Either String (JsonLikeValue, String)
parseListOfMoves ('e':r) acc = Right (JLMap acc, r)
parseListOfMoves s acc =
     case parseMove s of
                Right (i, r2) -> parseListOfMoves r2 (acc ++ i) 
                Left error -> Left error 
 
parseMove :: String -> Either String ([(String, JsonLikeValue)], String)
parseMove ('4':':':'p':'r':'e':'v':t)=
    case parse' t of
        Right (j,str)-> Right ([("prev", j)], str)
        Left error -> Left error
parseMove ('4':':':'l':'a':'s':'t':'d':t)=
    case parseLast t [] of
        Right (j,str)-> Right ([("last", j)], str)
        Left error -> Left error       
parseMove _ = Left "Not a correct move type"

parseLast :: String -> [(String, JsonLikeValue)] -> Either String (JsonLikeValue, String)
parseLast ('e':r) acc = Right (JLMap acc,r)
parseLast s acc =
     case parseValue s of
                Right (i, r2) -> parseLast r2 (acc ++ i) 
                Left error -> Left error 

parseValue :: String -> Either String ([(String, JsonLikeValue)], String)
parseValue ('2':':':'v':'s':'l': t) = 
    case parseChar t of
        Right (m, str) -> Right ([("vs", JLArray [JLString m])], str)
        Left error -> Left error
parseValue ('2':':':'x':'s':'l':t) = 
    case parseInt t of
        Right (m, str) -> Right ([("xs", JLArray [JLInt m])], str)
        Left error -> Left error
parseValue ('2':':':'y':'s':'l':t) = 
    case parseInt t of
        Right (m, str) -> Right ([("ys", JLArray [JLInt m])], str)
        Left error -> Left error
parseValue _ = Left "Incorrect value"

parseInt :: String -> Either String  (Int, String)
parseInt ('i':t) =
    let
        prefix = L.takeWhile C.isDigit t
        postfix = drop (length prefix) t
    in
        case postfix of
            ('e':'e':r) -> Right (read prefix, r)
            _ -> Left "Invalid integer"
parseInt _ = Left "Not an integer"

parseChar :: String -> Either String (String, String)
parseChar ('1':':':char:'e':t) = Right ([char],t)    
parseChar _ = Left "Not a Char"


--convert :: Int                      -- ^ Size of the matrix (number of columns or rows)
--        -> JsonLikeValue            -- ^ Parsed non-empty list of matrices
--        -> Either InvalidState To   -- ^ Converted matrix

--[(Int, Int, Char)]
convert :: Int -> JsonLikeValue -> Either InvalidState To
convert _ json = convert' json

--JLMap [("vs", JLArray [JLString "X"]), ("xs", JLArray [JLInt 2]), ("ys", JLArray [JLInt 3])]

convert' :: JsonLikeValue -> Either InvalidState To
convert' json =
    let answer = convertMove (dropMap json) []
        ((_, _, v):rest) = answer
    in
        case checkOrder v rest of
            False -> Left Order
            True -> 
                let msg = (sort answer)
                    ((x, y, _):rest) = msg
                in
                    case checkDuplicates (x, y) rest of
                        False -> Left Duplicates
                        True -> Right msg 


convertString :: JsonLikeValue -> String
convertString (JLArray [JLString v]) = v

convertInt :: JsonLikeValue -> Int
convertInt (JLArray [JLInt int]) = int

checkOrder :: Char -> [(Int, Int, Char)] -> Bool
checkOrder _ [] = True
checkOrder v ((_, _, v2):rest) = 
    if v == v2 then False else checkOrder v2 rest

sortY :: [(Int, Int, Char)] ->  To
sortY = sortBy (compare `on` (\(_,y,_)->y))

sortX :: [(Int, Int, Char)] ->  To
sortX = sortBy (compare `on` (\(x,_,_)->x))

checkDuplicates :: (Int, Int) -> [(Int, Int, Char)] -> Bool
checkDuplicates _ [] = True
checkDuplicates (x, y) ((x2, y2, _):rest) =
    if x == x2 && y == y2 then False else checkDuplicates (x2, y2) rest


dropMap :: (JsonLikeValue) -> [(String, JsonLikeValue)]
dropMap (JLMap x) = x
dropMap _ = error "JLMap"

convertMove :: [(String, JsonLikeValue)] -> To -> To
convertMove [("prev", json)] msg = convertMove (dropMap json) msg
convertMove [prev,("last", json)] msg = convertMove [prev] (convertMove' (dropMap json) [0][0]['v'] msg)
convertMove [("last", json),prev] msg = convertMove [prev] (convertMove' (dropMap json) [0][0]['v'] msg)
convertMove [("last", json)] msg = convertMove' (dropMap json) [0][0]['v'] msg

convertMove' :: [(String, JsonLikeValue)] -> [Int] -> [Int] -> [Char] -> To -> To
convertMove' [] x y v [] = (zip3 x y v)
convertMove' [] x y v to = ((zip3 x y v) ++ to)
convertMove' [(str, json), t, t2] [x][y][v] to = 
    case str of
        "xs" -> convertMove' [t, t2] [(convertInt json)] [y][v] to
        "ys" -> convertMove' [t, t2] [x] [convertInt json] [v] to
        "vs" -> convertMove' [t, t2] [x][y] (convertString json) to
convertMove' [(str, json), t] [x][y][v] to = 
    case str of
        "xs" -> convertMove' [t] [(convertInt json)] [y][v] to
        "ys" -> convertMove' [t] [x] [(convertInt json)] [v] to
        "vs" -> convertMove' [t] [x][y] (convertString json) to
convertMove' [(str, json)] [x][y][v] to = 
    case str of
        "xs" -> convertMove' [] [(convertInt json)] [y][v] to
        "ys" -> convertMove' [] [x] [(convertInt json)] [v] to
        "vs" -> convertMove' [] [x][y] (convertString json) to