-- The queens problem made famous by Wirth.
import Debug.Hoed.Stk

type Board = [Int]

main :: IO ()
main = runO $ observe "main" $
  if null solutions then putStrLn "no solution!"
  else putStr (showBoard (head solutions))
  where
  solutions = queens 4

queens :: Int -> [Board]
queens n' = observe "queens" (\n -> {-# SCC "queens" #-} valid n n) n'

valid :: Int -> Int -> [Board]
valid m' n' = observe "valid" (\m n -> {-# SCC "valid" #-} case m of
 0 -> [[]]
 m -> filter safe (extend n (valid (m-1) n))) m' n'

extend :: Int -> [Board] -> [Board]
extend n' bs' = observe "extend" (\n bs -> {-# SCC "extend" #-} consEach [1..n] bs) n' bs'

consEach :: Observable a => [a] -> [[a]] -> [[a]]
consEach xs' y' = observe "consEach" (\xs y -> {-# SCC "consEach" #-} case xs of
 []    -> []
 (a:x) -> map (a:) y ++ consEach x y) xs' y'

safe :: Board -> Bool
safe b' = observe "safe" (\(a:b) -> {-# SCC "safe" #-} no_threat a b 1) b'

no_threat :: Int -> Board -> Int -> Bool
no_threat a' bs' m' = observe "no_threat" (\a bs m -> {-# SCC "no_threat" #-} case bs of
 [] -> True
 (b:y) -> a /= b && a+m /= b && a-m /= b && no_threat a y (m+1)) a' bs' m'

showBoard :: Board -> String 
showBoard b' = observe "showBoard" (\b -> {-# SCC "showboard" #-}
  let rank r qcol =
        map line ["o o o", " \\|/ ", " === "]
        where
        line crown_slice =
          concat (zipWith square [1..] b)
          where
          square scol _ =
            if scol == qcol then crown_slice
            else if scol `rem` (2::Int) == r `rem` (2::Int) then "....."
            else "     "
  in unlines (concat (zipWith rank [1..] b))) b'
