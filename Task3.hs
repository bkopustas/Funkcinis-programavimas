module Task3
where
import System.IO
import Data.Char
import System.Exit
import Data.List
import System.Environment


data BadMessage = Syntax | Duplicate deriving (Show, Eq)

main :: IO ()
main = do
    args <- getArgs
    str <- getLine
    let ch = strToChar $ head args

    if str == "*" then do
        let (x, y, v) = makeFirstMove ch
            list = addMove [['-' | x <- [1.. 3]] | x <- [1.. 3]] (x, y) v
        putStr $ encode "" (x, y, v)
        drawBoard' list [['-' | x <- [1.. 3]] | x <- [1.. 3]]
        hPrint stderr ("My move: " ++ show (x, y, v))
      else do
            case parse str of
                  Right list -> do
                        if isGameOver list then do
                                    putStr str
                                    hPrint stderr "Game: "
                                    drawBoard list
                                    if checkRow list 'X' || checkCol list 'X' || checkDiag list 'X' then do
                                          hPrint stderr "X already won"
                                          exitWith (ExitFailure 20)
                                    else if checkRow list 'O' || checkCol list 'O' || checkDiag list 'O' then do
                                          hPrint stderr "O already won"
                                          exitWith (ExitFailure 20)
                                    else do
                                          hPrint stderr "Already a draw"
                                          exitWith (ExitFailure 20)
                        else do
                              let
                                    (x,y,v) = findBestMove list (ch == 'X')
                                    list1 = addMove list (x,y) v
                              putStr $ encode str (x,y,v)
                              hPrint stderr $ "Game: "
                              drawBoard' list list1
                              hPrint stderr (" My Move: " ++ show (x,y,v))
                              if isGameOver list1 then 
                                    if checkRow list1 'X' || checkCol list1 'X' || checkDiag list1 'X' then do
                                          hPrint stderr " X wins"
                                          if ch == 'X' then 
                                                exitWith (ExitFailure 10) -- I won
                                          else
                                                exitWith (ExitFailure 11) -- I lost
                                    else if checkRow list1 'O' || checkCol list1 'O' || checkDiag list1 'O' then do
                                          hPrint stderr "O wins"
                                          if ch == 'O' then 
                                                exitWith (ExitFailure 10) 
                                          else
                                                exitWith (ExitFailure 11)
                                    else do
                                          hPrint stderr "Draw"
                                          exitWith (ExitFailure 12)
                              else do
                                    hPrint stderr "in game"
                                    exitSuccess
                  Left err ->
                        case err of
                              Syntax -> do
                                    hPrint stderr "Syntax error"
                                    exitWith (ExitFailure 100)
                              Duplicate -> do
                                    hPrint stderr "Two moves to the same cell"
                                    exitWith (ExitFailure 101)

drawBoard :: [[Char]] -> IO()
drawBoard (a:b:c:_) = do
      hPrint stderr (drawRow a)
      hPrint stderr "----------"
      hPrint stderr (drawRow b)
      hPrint stderr "----------"
      hPrint stderr (drawRow c)

drawBoard' :: [[Char]] -> [[Char]] -> IO()
drawBoard' (a:b:c:_) (a1:b1:c1:_) = do
      hPrint stderr (drawRow a ++ "      " ++ drawRow a1)
      hPrint stderr "----------     ----------"
      hPrint stderr (drawRow b ++ "  ->  " ++ drawRow b1)
      hPrint stderr "----------     ----------"
      hPrint stderr (drawRow c ++ "      " ++ drawRow c1)

drawRow :: [Char] -> String
drawRow row = intercalate " | " (map (:[]) row)



strToChar :: String -> Char
strToChar (a : _) = a

makeFirstMove :: Char -> (Int, Int, Char)
makeFirstMove v = (0, 0, v)

encode :: String -> (Int, Int, Char) -> String
encode [] (x, y, v) = concat ["d4:lastld4:datali", show x, "ei", show y, "e1:", [v], "eee", "e"]
encode str (x, y, v) = concat ["d4:prev", str, "4:lastld4:datali", show x, "ei", show y, "e1:", [v], "eee", "e"]

---PARSE---

parse :: String -> Either BadMessage [[Char]]
parse msg = 
      case parseListOfMoves msg [['-' | x <- [1.. 3]] | x <- [1.. 3]] of
            Right (ats, _) -> Right ats
            Left err -> Left err

parseListOfMoves :: String -> [[Char]] -> Either BadMessage ([[Char]], String)
parseListOfMoves ('d':r) v = 
      case parseListOfMoves' r v of
            Right (m, str) -> Right (m, str)
            Left err -> Left err
parseListOfMoves _ _ = Left Syntax --exitWith (exitFailure 100)  

parseListOfMoves' :: String -> [[Char]] -> Either BadMessage ([[Char]], String)
parseListOfMoves' ('e': r) acc = Right (acc, r)
parseListOfMoves' s acc =
      case parseMove s acc of
                  Right (i, r) -> parseListOfMoves' r i
                  Left error -> Left error


parseMove :: String -> [[Char]] -> Either BadMessage ([[Char]], String)
parseMove ('4':':':'p':'r':'e':'v':t) acc = 
      case parseListOfMoves t acc of
            Right (m, n) -> Right (m, n)
            Left err -> Left err
parseMove ('4':':':'l':'a':'s':'t':'l':'d':'4':':':'d':'a':'t':'a':'l':'i':x:'e':'i':y:'e':'1':':':v:'e':'e':'e':t) acc = 
      case parseMove' (digitToInt x) (digitToInt y) v acc of
            Right ats -> Right (ats, t)
            Left err -> Left err
parseMove _ _ = Left Syntax

parseMove' :: Int -> Int -> Char -> [[Char]] -> Either BadMessage [[Char]]
parseMove' x y v board
      |(a, replace:b) <- splitAt y board = 
            case parseMove'' x v replace of
                  Right ats -> Right (a++ ats:b)
                  Left err -> Left err
            

parseMove'' :: Int -> Char -> [Char] -> Either BadMessage [Char]
parseMove'' x v row 
      |(a, replace:b) <- splitAt x row =
            if replace == '-' then Right (a ++ v:b) else Left Duplicate

---MOVE-LOGIC---

isFull :: [[Char]] -> Bool
isFull = all (notElem '-')

isGameOver :: [[Char]] -> Bool
isGameOver board = isFull board || 
            (checkRow board 'X' || checkCol board 'X'|| checkDiag board 'X') ||
            (checkRow board 'O' || checkCol board 'O'|| checkDiag board 'O')


checkRow :: [[Char]] -> Char -> Bool
checkRow [] _ = False
checkRow ((a:b:c:_):t) char =
      (a==char && a==b && b==c && a==c) || checkRow t char


checkCol :: [[Char]] -> Char -> Bool
checkCol (a : b : c : _) = checkColumn a b c

checkColumn :: [Char] -> [Char] -> [Char] -> Char -> Bool
checkColumn [] [] [] _ = False
checkColumn a b c char = (head a == head b && head a == head c && head a == char) ||
     checkColumn (drop 1 a) (drop 1 b) (drop 1 c) char

checkDiag :: [[Char]] -> Char -> Bool
checkDiag (a : b : c : _) char = 
    (head a == b !! 1 && head a == c !! 2 && head a == char) ||
    (a !! 2 == b !! 1 && a !! 2 == head c && head c == char)

findBestMove :: [[Char]] -> Bool -> (Int, Int, Char)
findBestMove moves maxPlayer
    | maxPlayer =
        let 
            possMoves = possibleMoves moves 0
            allBoards = map (\m -> addMove moves m 'X') possMoves -- <-- puts X in every possible position
            allOutcomes = map (`minimax` False) allBoards -- <-- calls minimax on each board, makes list of each board outcome (-1, 0, 1)
            Just indx = elemIndex (maximum allOutcomes) allOutcomes
            (x, y) = possMoves !! indx
        in
            (x, y, 'X')
    | otherwise = (x, y, 'O')
        where
            possMoves = possibleMoves moves 0
            allBoards = map (\m -> addMove moves m 'O') possMoves -- <-- puts O in every possible position
            allOutcomes = map (`minimax` True) allBoards -- <-- calls minimax on each board, makes list of each board outcome (-1, 0, 1)
            Just indx = elemIndex (minimum allOutcomes) allOutcomes
            (x, y) = possMoves !! indx

minimax :: [[Char]] -> Bool -> Int
minimax moves maxPlayer
    | isGameOver moves =
        if checkRow moves 'X' || checkCol moves 'X' || checkDiag moves 'X' then 1
        else if checkRow moves 'O' || checkCol moves 'O' || checkDiag moves 'O' then -1
        else 0
    | maxPlayer =
        let
            possMoves = possibleMoves moves 0
            allBoards = map (\m -> addMove moves m 'X') possMoves
            allOutcomes = map (`minimax` False) allBoards
        in
            maximum allOutcomes
    | otherwise = minimum allOutcomes
        where 
            possMoves = possibleMoves moves 0
            allBoards = map (\m -> addMove moves m 'O') possMoves
            allOutcomes = map (`minimax` True) allBoards

possibleMoves :: [[Char]] -> Int -> [(Int, Int)]
possibleMoves _ 3 = []
possibleMoves ([a, b, c] : t) acc = moves ++ possibleMoves t (acc + 1)
    where
        moves = [(0, acc) | a == '-'] ++
                [(1, acc) | b == '-'] ++
                [(2, acc) | c == '-']

addMove :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
addMove (a : b : c : _) (x, y) char
    | y == 0 = [addToRow a x char, b, c]
    | y == 1 = [a, addToRow b x char, c]
    | y == 2 = [a, b, addToRow c x char]


addToRow :: [Char] -> Int -> Char -> [Char]
addToRow row x char = take x row ++ [char] ++ drop (x + 1) row