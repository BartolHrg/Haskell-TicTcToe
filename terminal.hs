import System.Process
import Data.Map
import Data.Char
import System.IO

clear = system "cls" 

turns :: [Char]
turns = 'x':'o':turns

board = empty 
change [r, c] board t = insert (ord r - ord 'a', ord c - ord '1') t board 
getc (r, c) = findWithDefault ' ' (r, c)
get [r, c] = findWithDefault ' ' (ord r - ord 'a', ord c - ord '1')

printBoard board = do 
    putStrLn "  1 2 3"
    putStr "a "
    putChar (getc (0, 0) board) >> putChar '│' >> putChar (getc (0, 1) board) >> putChar '│' >> putChar (getc (0, 2) board) 
    putChar '\n' 

    putStrLn "  ─┼─┼─"
    
    putStr "b "
    putChar (getc (1, 0) board) >> putChar '│' >> putChar (getc (1, 1) board) >> putChar '│' >> putChar (getc (1, 2) board) 
    putChar '\n' 

    putStrLn "  ─┼─┼─"
    
    putStr "c "
    putChar (getc (2, 0) board) >> putChar '│' >> putChar (getc (2, 1) board) >> putChar '│' >> putChar (getc (2, 2) board) 
    putChar '\n' 


checkID [r, c] = (elem r "abc") && (elem c "123")

checkRows board 
    | Prelude.null res  = ' '
    | otherwise         = head res
    where 
        res = Prelude.filter (/= ' ') [e | i <- [0..2], let e = getc (i, 0) board, all (== e) [getc (i, j) board | j <- [1..2]]]
checkColums board
    | Prelude.null res  = ' '
    | otherwise         = head res
    where 
        res = Prelude.filter (/= ' ') [e | j <- [0..2], let e = getc (0, j) board, all (== e) [getc (i, j) board | i <- [1..2]]]
checkDiagonals board
    | e == ' '                                      = ' '
    | get "a1" board == e && e == get "c3" board    = e
    | get "a3" board == e && e == get "c1" board    = e
    | otherwise                                     = ' '
    where
        e = get "b2" board
checkWin board 
    | r /= ' '  = r
    | c /= ' '  = c
    | d /= ' '  = d
    | otherwise = ' '
    where 
        r = checkRows board 
        c = checkColums board 
        d = checkDiagonals board

-- loop _ 0 = return ()
loop (t:turns) board msg = do
    clear >> printBoard board
    let w = checkWin board
    if w /= ' ' then do
        putStr "Thw winner is " >> putChar (toUpper w) >> putChar '\n'
    else do -- continue

    putStr msg
    putStr "please player " >> putChar (toUpper t) >> putStr " enter row-column id (e.g. a1) "
    hFlush stdout 
    id <- getLine 

    if checkID id then
        loop turns (change id board t) ""
    else
        loop (t:turns) board "that's wrong input!\n"

main = do
    loop turns board ""
