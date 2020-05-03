

import System.IO.Unsafe
import Data.List
import Control.Exception



trace :: Show a => String -> a -> a 
trace s x = unsafePerformIO ( do
                                putStrLn s
                                putStrLn $ "result " ++ show x
                                return x
                               )
-- trace _ x = x

data Tree a = Node a [Tree a]
data MinMax = Min | Max
  deriving (Eq,Read)

-- minimax algorithm with alpha-beta pruning

minimaxAB :: (Ord a, Bounded a) => Tree a -> MinMax -> Tree a
minimaxAB tree mode = minimaxAB' tree minBound maxBound mode
  
minimaxAB' :: (Ord a, Bounded a) => Tree a -> a -> a -> MinMax -> Tree a
minimaxAB' (Node a []) _ _ _ = Node a []
minimaxAB' (Node a ts) alpha beta mode
  | mode == Min =
      let (v,ts') = loop1 ts maxBound [] alpha beta
      in (Node v ts')
  | mode == Max =
      let (v,ts') = loop2 ts minBound [] alpha beta
      in (Node v ts')
  where
    loop1 [] v accum alpha beta   = (v, reverse accum)
    loop1 (x:xs) v accum alpha beta = 
      assert( alpha < beta ) $ 
      let (Node w ts)  = minimaxAB' x alpha beta Max
          v' = min v w
          beta' = min beta v'
          -- cut off if v <= alpha
          xs' = if beta' <= alpha then [] else xs  
      in loop1 xs' v' ((Node w ts) : accum)  alpha beta'
      
    loop2 [] v accum alpha beta   = (v, reverse accum)
    loop2 (x:xs) v accum alpha beta = 
      assert( alpha < beta ) $ 
      let (Node w ts) = minimaxAB' x alpha beta Min
          v' = max v w
          alpha' = max alpha v'
          -- cut off if beta <= v
          xs' = if beta <= alpha' then [] else xs 
      in loop2 xs' v' ((Node w ts) : accum)  alpha' beta
        

putTree :: Show a => Tree a -> IO ()
putTree tree = helper 0 tree
  where
    helper depth (Node n ts) = do
      let indent = replicate (depth * 2) ' '
      putStrLn $ indent ++ (show n)
      sequence_ ( map (helper (depth + 1)) ts )

tree1 :: Tree Int
tree1 = ( Node 0 [
            Node 0 [
                Node 1 [],
                Node 3 []
                ],
            Node 0 [
                Node 4 [],
                Node 4 [],
                Node 1 [],
                Node 5 []
                ]
            ]
        )

tree2 :: Tree Int
tree2 = ( Node 0 [
            Node 0 [
                Node 5 [],
                Node 0 [
                    Node 4 [],
                    Node 4 []
                    ]
                ],
            Node 0 [
                Node 1 [],
                Node 0 [
                    Node 3 [],
                    Node 0 [
                        Node 6 [],
                        Node 5 [],
                        Node 4 [],
                        Node 3 [],
                        Node 2 []
                           ]
                    ],
                Node 4 [],

                Node 5 []
                ]
            ]
        )



        
testTree :: Tree Int -> MinMax -> IO ()
testTree tree mode = do
  putTree tree
  putTree (minimaxAB tree mode)

  
  
