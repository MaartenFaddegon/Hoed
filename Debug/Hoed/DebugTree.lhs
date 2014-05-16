> module Debug.Hoed.DebugTree 
> ( Tree
> , addNodes
> , addRoot
> , addEdge
> , debugSession
> )
> where
> 
> import Data.Map.Strict(Map, (!))
> import qualified Data.Map.Strict as Map
> import System.IO
> import Data.List(nub)
> import System.Process(runCommand)
> import qualified Graphics.UI.Threepenny as UI
> import Graphics.UI.Threepenny (startGUI,defaultConfig,tpPort,tpStatic
>                               , Window, UI, (#), (#+), string, on
>                               )

---------------------------------------------------------------------------
-- Our data types.

> data Tree a = Tree [a] (Map a (Node a))
>
> data Node a = Node { value    :: a 
>                    , children :: [a]
>                    , i        :: Int
>                    , status   :: Status
>                    } 
>               deriving Show
>
> data Status = Correct | Wrong | Unassessed
>               deriving (Show, Eq)

---------------------------------------------------------------------------
-- Showing trees

> instance (Ord a, Show a) => Show (Tree a) where
>   show (Tree rs map) =  "digraph G {\n"
>                      ++ foldl (\acc x -> acc ++ (showNode map x)) "" es
>                      ++ showRootEdges map rs
>                      ++ foldl (\acc x -> acc ++ (showOtherEdges map x)) "" es
>                      ++ "}\n"
>     where (_,es) = unzip (Map.toList map)
>
> showNode :: (Ord a, Show a) => (Map a (Node a)) -> Node a -> String 
> showNode map (n@Node{value=v,i=i,children=cs,status=s}) 
>   =  "  " ++ node i ++ " [label=\"" ++ show s ++ buggy
>   ++ ": " ++ (escape . show) v ++ "\"]\n"
>   where node i = "node" ++ show i
>         escape []         = []
>         escape ('"' : ss) = '\\' : '"' : escape ss
>         escape (s   : ss) = s : escape ss
>         buggy            = if isBuggy map n then " (buggy)" else ""

>
> showRootEdges :: (Ord a, Show a) => (Map a (Node a)) -> [a] -> String
> showRootEdges map rs
>  = foldl (\acc r -> acc ++ edgeTo r  ++ "\n") "" rs
>  where edgeTo r = let Node{i=j} = map ! r
>                    in "  root -> node" ++ show j
>
> showOtherEdges :: (Ord a, Show a) => (Map a (Node a)) -> Node a -> String 
> showOtherEdges map (Node{value=v,i=i,children=cs}) 
>   = foldl (\acc w -> acc ++ edgeTo w  ++ "\n") "" cs
>   where node i = "node" ++ show i
>         edgeTo w = let Node{i=j} = map ! w
>                    in "  " ++ node i ++ "->" ++ node j 
>                    --   ++ " // " ++ show w

---------------------------------------------------------------------------
-- Constructing trees.

> newNode :: a -> Node a
> newNode v = Node {value=v, status=Unassessed, children=[], i=0}

MF TODO: need to find out why we need to nub here!

> addChild :: Eq a => a -> Node a -> Node a
> addChild c (e@Node{children=cs}) = e{children = nub (c:cs)}

Create a debug tree with no edges from a list of nodes.
 
> addNodes :: Ord a => [a] -> Tree a
> addNodes ns = Tree [] (Map.fromList $ zip ns es)
>  where es' = map newNode ns
>        es  = map (\(i,e) -> e{i=i}) (zip [1..] es')

Add an edge between the existing values v and w.

> addEdge :: Ord a => a -> Tree a -> a -> Tree a
> addEdge v (Tree rs map) w = Tree rs $ Map.adjust (addChild w) v map

> addRoot :: Tree a -> a -> Tree a
> addRoot (Tree rs map) v = Tree (v:rs) map

> markNode :: Ord a => Tree a -> a -> Status -> Tree a
> markNode (Tree rs map) v s = Tree rs $ Map.adjust (setStatus s) v map

> setStatus :: Status -> Node a -> Node a
> setStatus s n = n{status=s}

> isUnassessed :: Ord a => Tree a -> a -> Bool
> isUnassessed (Tree _ map) v = let Node{status=status} = map ! v 
>                               in  status == Unassessed

> getChildren :: Ord a => Tree a -> a -> [a]
> getChildren (Tree _ map) v = children
>       where Node{children=children} = map ! v

--------------------------------------------------------------------------
-- Algorithmic debugging

> debugSession :: (Show a, Ord a) => Tree a -> IO (Tree a)
> debugSession (tree@(Tree rs _)) -- = debugSession' tree rs
>   = do startGUI defaultConfig
>            { tpPort       = Just 10000
>            , tpStatic     = Just "./wwwroot"
>            } (debugSession' tree rs)
>        return tree -- MF TODO: now we just return the same tree!

> debugSession' :: (Show a, Ord a) => Tree a -> [a] -> Window -> UI ()
> debugSession' tree worklist window
>   = do return window # UI.set UI.title "Hello World!"
>        b <- UI.button # UI.set UI.text "Hoi!"
>        UI.getBody window #+ [UI.element b]
>        on UI.click b $ const $ do UI.element b # UI.set UI.text "I have been clicked!"     

TODO:  debugSession' tree [] 
TODO:   = 
TODO:  debugSession' tree (w:worklist)
TODO: 
TODO: 
TODO:  setup :: Window -> UI ()
TODO:  setup window = do
TODO:   = do if buggyFound 
TODO:        then done tree
TODO:        else do -- Render the tree as it is now.
TODO:                writeFile "debugTree.dot" (show tree)
TODO:                runCommand "dot -Tpng debugTree.dot > debugTree.png"
TODO: 
TODO: 
TODO: 
TODO:                putStrLn (show w)
TODO:                if isUnassessed tree w then ask else skp
TODO:   where
TODO:     ask = do putStr "[c/w]? "
TODO:              hFlush stdout
TODO:              answer <- getLine
TODO:              case answer of
TODO:                "c" -> debugSession' (markNode tree w Correct) worklist
TODO:                "w" -> debugSession' (markNode tree w Wrong) 
TODO:                                     (worklist ++ getChildren tree w)
TODO:     skp = do putStrLn "Skipping: already assessed."
TODO:              debugSession' tree worklist
TODO:     buggyFound = findBuggy tree /= []


Search for nodes that are marked as wrong, but which children are
all correct.

> findBuggy :: (Show a, Ord a) => Tree a -> [a]
> findBuggy (Tree _ m) = bs
>  where (_,ns)  = (unzip . Map.toList) m
>        bns     = filter (isBuggy m) ns
>        bs      = map (\Node{value=v} -> v) bns
>
> isBuggy :: (Show a, Ord a) => (Map a (Node a)) -> Node a -> Bool
> isBuggy m Node{status=Wrong, children=cs} 
>   = cs == [] || ws == []
>   where ws        = filter isWrong cs
>         isWrong c = let Node{status=s} = m ! c in s /= Correct
> isBuggy _ _ = False
>


> done tree = do putStrLn $ "Done, buggy nodes are: \n" ++ show (findBuggy tree)
>                return tree
