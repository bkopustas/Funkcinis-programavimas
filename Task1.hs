module Task1 where
import Task1Message

import Data.Char (digitToInt)

parse :: Int -> String -> From
parse _ ('d':str) = (ys,xs,vs)
    where
        s = parser str
        (xs', s1) = parseListofChars s 
        s2 = parser' s1
        (ys', s3) = parseListofChars s2
        s4 = parser'' s3
        (vs, _) = parseListofChars s4
        xs = map digitToInt xs'
        ys = map digitToInt ys'  


parser :: String -> String
parser ('2':':':'y':'s':str) = str

parser' :: String -> String
parser' ('2':':':'x':'s':str) = str

parser'' :: String -> String
parser'' ('2':':':'v':'s':str) = str


parseChar :: String -> (Char, String)
parseChar ('1':':':ch:t) = (ch,t)

parseListofChars :: String -> ([Char], String)
parseListofChars ('l':r) = parseListofChars' r []
parseListofChars _ = error "Not a List"

parseListofChars' :: String -> [Char] -> ([Char], String)
parseListofChars' ('e':r) acc = (acc, r)
parseListofChars' s acc =
    let
        (i, r) = parseChar s
    in
        parseListofChars' r (acc ++ [i])

--convert :: Int  -> ([Int], [Int], [Char]) -> [[(Int, Char)]]
convert :: Int -> From -> To
convert size parsedMessage = convert' parsedMessage [[]| _ <-[1..size]]

--convert' :: ([Int], [Int], [Char]) -> [[(Int, Char)]] -> [[(Int, Char)]]
convert' :: From -> To -> To
convert' ([],[],[]) msg = msg
convert' (x:xs, y:ys, v:vs) msg = convert' (xs,ys,vs) (fillMatrix y msg (zip [x] [v]))

fillMatrix :: Int -> [[a]] -> [a] -> [[a]]
fillMatrix p matrix val
    | (up, t:l) <- splitAt p matrix = up ++ [(t ++ val)] ++ l