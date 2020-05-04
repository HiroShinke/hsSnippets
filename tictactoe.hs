

-- Tictactoe: baseed on Grahum Hutton's book 

import Data.Char
import Data.List
import System.IO
import System.IO.Unsafe
import System.Random (randomRIO)
import Text.Printf
import Control.Exception


trace :: Show a => String -> a -> a
{-
trace s x = unsafePerformIO ( do
                                putStrLn s
                                putStrLn $ "result " ++ show x
                                return x
                               )
-}
trace _ x = x


size :: Int
size = 3

type Grid = [[Player]]

data Player = A | O | B | X | Z
  deriving (Eq, Ord, Show,Bounded)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

-- turn :: Grid -> Player
-- turn g = if os <= xs then O else X
--   where
--     os = length (filter (==O) ps)
--     xs = length (filter (==X) ps)
--     ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (p ==)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]


transpose2 :: [[a]] -> [[a]]
transpose2 [[]]   = [[]]
transpose2 (r:rs) = zipWith (:) r rs'
  where
    r0 = map head rs
    sg  = map tail rs
    rs' = r0 : transpose2 sg

diag :: [[a]] -> [a]
diag []   = []
diag (r:rs) = head r : diag (map tail rs)

won :: Grid -> Bool
won g = wins O g || wins X g

-------------

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [ replicate ((size*4)-1) '-' ]

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
--    bar = replicate 3 "|"
    bar = ["|"]
    
showPlayer :: Player -> [String]
--showPlayer O = ["   ", " O ", "   "]
--showPlayer X = ["   ", " X ", "   "]
--showPlayer B = ["   ", "   ", "   "]

showPlayer O = [" O "]
showPlayer X = [" X "]
showPlayer B = ["   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x (y:ys) | null ys = [y]
                    | otherwise =  y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i
  then [ chop size $ replaceAt i p (concat g) ]
  else []
  where
    replaceAt i x xs = map (replace i x) (zip [0..] xs)
    replace i x (i',x') = if i == i' then x else x'
    
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)


getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs
                     then
                     return (read xs)
                     else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt
                        
tictactoe :: IO ()
tictactoe = run empty O


run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p


run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g   = putStrLn "It's a draw!\n"
         | otherwise =
             do i <- getNat (prompt p)
                case move g i p of
                  [] -> do putStrLn "ERROR: Invalid move"
                           run' g p
                  [g'] -> run g' (next p)

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++"H")

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
  deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g children
  where
    children = [ gametree g' (next p) | g' <- moves g p ]
    
moves :: Grid -> Player -> [Grid]
moves g p | won g  = []
          | full g = []
          | otherwise = [ g' | i <- [0..size^2], g' <- move g i p ]

prune :: Int -> Tree a -> Tree a
prune 0 (Node a _)  = Node a []
prune n (Node a ts) = Node a (map (prune (n-1)) ts)

depth :: Int
depth = 9

minimax :: Tree Grid -> Player -> Tree (Grid,Player)
minimax (Node g []) turn
  | wins O g  = Node (g,O) []
  | wins X g  = Node (g,X) []
  | otherwise = Node (g,B) []
minimax (Node g ts) turn
  | turn == O = Node (g,minimum ps) ts'
  | turn == X = Node (g,maximum ps) ts'
  where
    ts' = [ minimax t (next turn) | t <- ts ]
    ps  = [ p | Node (_,p) _  <- ts' ]


minimaxAB :: Tree Grid -> Player -> Tree (Grid,Player)
minimaxAB tree turn = minimaxAB' tree minBound maxBound turn


minimaxAB' :: Tree Grid -> Player -> Player -> Player -> Tree (Grid,Player)
minimaxAB' (Node g []) _ _ _
  | wins O g  = Node (g,O) []
  | wins X g  = Node (g,X) []
  | otherwise = Node (g,B) []
minimaxAB' (Node g ts) alpha beta turn =
  let (v,ts') = if turn == O
                 then loop1 ts maxBound [] alpha beta
                 else loop2 ts minBound [] alpha beta
  in
    Node (g,v) ts'
  where
    loop1 []     v accum alpha beta = (v,reverse accum)
    loop1 (x:xs) v accum alpha beta = 
      let (Node (g',w) ts')  = minimaxAB' x alpha beta (next turn)
          v' = min v w
          beta' = min beta v'
          -- Ease condition for pruning,
          -- to leave nodes which have equal value.
          xs' = if beta'  < alpha then [] else xs
      in loop1 xs' v' (Node (g',w) ts' : accum) alpha beta' 

    loop2 []     v accum alpha beta = (v,reverse accum)
    loop2 (x:xs) v accum alpha beta = 
      let (Node (g',w) ts') = minimaxAB' x alpha beta (next turn)
          v' = max v w
          alpha' = max alpha v'
          xs' = if beta < alpha' then [] else xs
      in loop2 xs' v' (Node (g',w) ts' : accum) alpha' beta


bestmove :: Grid -> Player -> Grid
bestmove g p = head (bestmoves g p)

bestmove2 :: Grid -> Player -> IO Grid
bestmove2 g p = randomChoice (bestmoves g p)


bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = moves'
  where
    tree = prune depth (gametree g p)
    Node (_,best) ts' = minimaxAB tree p
    player (Node (_,p') _) = p'
    ts'' = [ t | t <- ts', player t == best ]
    minDepth = minimum $ map (countMinDepth p) ts''
    ts''' = [ t | t <- ts'', countMinDepth p t == minDepth ]
    moves' = [ g' | Node (g',p') _ <- ts''' ]

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          p <- getPlayer "Please select first player: "
          play empty p


getPlayer :: String -> IO Player
getPlayer prompt = do putStr prompt
                      xs <- getLine
                      case xs of
                        "O" -> return O
                        "X" -> return X
                        otherwise -> do putStrLn "ERROR: invalid player"
                                        getPlayer prompt
                        
play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1,1)
  putGrid g
  play' g p
  
play' :: Grid -> Player -> IO ()
play' g p | wins O g = putStrLn "Player O wins!\n"
          | wins X g = putStrLn "Player X wins!\n"
          | full g   = putStrLn "It's a draw!\n"
          | p == O   = 
              do i <- getNat (prompt p)
                 if size^2 <= i
                   then
                   do
                     dumpDebug g p
                     play g p
                   else
                   case move g i p of
                     [] -> do putStrLn "ERROR: Invalid move"
                              play' g p
                     [g'] -> play g' (next p)
          | p == X   =
              do putStrLn "Player X is now thinking ... "
                 g' <- bestmove2 g p
                 (play $! g') (next p)

---- for debuging

dumpDebug :: Grid -> Player -> IO ()
dumpDebug g p = do
  dumpGameTree (minimax (gametree g p) p) "dump1.txt" 
  dumpGameTree (minimaxAB (gametree g p) p) "dump2.txt" 
  
dumpGameTree :: Tree (Grid,Player) -> String -> IO ()
dumpGameTree tree path = do
  hOut <- openFile path WriteMode
  hPutGameTree tree hOut
  hClose hOut

hPutGameTree :: Tree (Grid,Player) -> Handle -> IO ()
hPutGameTree tree hOut = do
  helper 0 tree
  where 
    helper depth (Node (g,p) ts) = 
      if depth < 10
      then
        do
          let indent = replicate (depth * 4) ' '
          let minMax = if depth `mod` 2 == 0 then "max: " else "min: "
          hPutStrLn hOut $ indent ++
            (printf "%02d: " (depth::Int)) ++
            minMax ++ (show p) ++ ": " ++ (show g)
          sequence_ ( map (helper (depth + 1)) ts )
      else
        return ()
        
putGridDepth :: Grid -> String -> IO ()
putGridDepth g indent =
  ( putStrLn . unlines . map (indent++) . concat . interleave bar . map showRow ) g
  where
    bar = [ replicate ((size*4)-1) '-' ]


-- Exercise 1

values :: Tree a -> [a]
values (Node a ts) = [a] ++ concat (map values ts) 

countDepth :: Tree a -> Int
countDepth (Node a []) = 1
countDepth (Node a ts) = 1 + maximum (map countDepth ts)

-- Exercise 2

randomChoice :: [a] -> IO a
randomChoice xs = do
  n <- randomRIO (0, length xs - 1)
  return (xs !! n)
  
-- Excercise 3

countMinDepth :: Player -> Tree (Grid,Player) -> Int
countMinDepth p tree =
  trace "coutMinDepth=" $ helper 0 tree 
  where
    helper depth (Node (g,p') ts)
      | null ts && p == p'   = depth
      | null ts              = maxBound
      | otherwise            = minimum (map (helper (depth + 1)) ts )

-- Excercise 4


