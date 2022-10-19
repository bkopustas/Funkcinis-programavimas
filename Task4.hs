module Main where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import System.Exit
import System.Environment
import Data.List as L
import Data.Char as C

type Parser a = ExceptT String (State String) a
data JsonLikeValue = JLString String | JLInt Int | JLMap [(JsonLikeValue, JsonLikeValue)] | JLArray [JsonLikeValue] deriving (Show, Eq)

main :: IO ()
main = do
    putStrLn "Enter the message: "
    msg <- getLine
    case runState (runExceptT parseAny) msg of
        (Right json, str) -> do --if null str
            --then do
                let top5 = findTop json "root" []
                showAnswer top5
                --mapM_ (\(path, value) -> putStrLn (path  ++ " = " ++ show value)) top5
                exitSuccess
            -- else do
            --     putStrLn "Failed to parse message"
            --     exitFailure
        (Left err, str) -> do
            putStrLn err
            exitFailure
    exitSuccess 

showAnswer :: [(String, Int)] -> IO()
showAnswer [] = return ()
showAnswer ((path, value):t) = do
    putStrLn (path ++ " = " ++ show value)
    showAnswer t

----TOP5----

findTop :: JsonLikeValue -> String -> [(String, Int)] -> [(String, Int)]
findTop json path top5 =
    case json of
        JLString str -> top5
        JLInt int -> insertInt (path, int) top5 []
        JLArray arr -> findTop' arr path 0 top5
        JLMap m -> findTop'' m path top5

findTop' :: [JsonLikeValue] -> String -> Int -> [(String, Int)] -> [(String, Int)]
findTop' [] _ _ top5 = top5
findTop' (h:t) path place top5 = 
    let newTop5 = findTop h (concat [path, "[", show place, "]"]) top5
    in findTop' t path (place + 1) newTop5

findTop'' :: [(JsonLikeValue, JsonLikeValue)] -> String -> [(String, Int)] -> [(String, Int)]
findTop'' [] _ top5 = top5
findTop'' ((JLString key, value):t) path top5 =
    let newTop5 = findTop value (concat[path, ".", key]) top5
    in findTop'' t path newTop5

insertInt :: (String, Int) -> [(String, Int)] -> [(String, Int)] -> [(String, Int)]
insertInt tuple [] acc = 
    if length acc < 5
        then acc ++ [tuple]
        else acc
insertInt (p, int) ((p2, int2):t) acc =
    if int >= int2
        then insertInt (p2, int2) t (acc ++ [(p, int)])
        else insertInt (p, int) t (acc ++ [(p2, int2)])

----PARSE----

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

parseAny :: Parser JsonLikeValue
parseAny = parseInt  <|> parseString <|> parseList <|> parseJson

parseInt :: Parser JsonLikeValue
parseInt = do
    str <- lift get
    JLInt <$> parseInt' str

parseString :: Parser JsonLikeValue
parseString = do
    str <- lift get 
    JLString <$> parseString' str

parseList :: Parser JsonLikeValue
parseList = do
     str <- lift get
     case str of
         ('[':t) -> do
             lift $ put t
             items <- parseList' t False [] 
             return $ JLArray items
         _ -> throwE "Bad list"

parseJson :: Parser JsonLikeValue
parseJson = do
    str <- lift get 
    case str of
        ('{':t) -> do
            lift $ put t
            items <- parseJson' t False [] 
            return $ JLMap items
        _ -> throwE "Bad json"

parseInt' :: String -> Parser Int
parseInt' t = do
    let (sk, r) = span isDigit t
    case sk of
        "" -> throwE "Invalid int"
        _ -> do
            lift $ put r
            return (read sk)

parseString' :: String -> Parser String
parseString' ('"':t) = do
    let s = L.takeWhile (/= '"') t
    let rest = drop (length s) t
    case rest of
        ('"':r) -> do
            lift $ put r
            return s
        _ -> throwE "Invalid String"
parseString' _ = throwE "Not a string"

parseList' :: String -> Bool -> [JsonLikeValue] -> Parser [JsonLikeValue]
parseList' (']':t) _ acc = do
     lift $ put t
     return acc
parseList' (',':t) _ acc = do
    lift $ put t
    i <- parseAny
    r2 <- lift get
    parseList' r2 True (acc ++ [i])
parseList' t _ acc = do
    i <- parseAny
    r2 <- lift get
    parseList' r2 True (acc ++ [i])

parseJson' :: String -> Bool -> [(JsonLikeValue, JsonLikeValue)] -> Parser [(JsonLikeValue, JsonLikeValue)]
parseJson' ('}':t) _ acc = do
    lift $ put t
    return acc
parseJson' (',':t) expectComma acc = do
    lift $ put t
    i <- parseJsonItem
    r2 <- lift get
    parseJson' r2 True (acc ++ [i])
parseJson' s expectComma acc = do
    i <- parseJsonItem
    r2 <- lift get
    parseJson' r2 True (acc ++ [i])

parseJsonItem :: Parser (JsonLikeValue, JsonLikeValue)
parseJsonItem = do
    key <- parseString
    next <- lift get
    temp <- lift $ put (drop 1 next)
    value <- parseAny
    return (key, value)