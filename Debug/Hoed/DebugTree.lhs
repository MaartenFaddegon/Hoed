> {-# LANGUAGE CPP#-}
> module Debug.Hoed.DebugTree 
> ( Tree
> , Labeled(..)
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
> import System.Process(system)
> import qualified Graphics.UI.Threepenny as UI
> import Graphics.UI.Threepenny (startGUI,defaultConfig,tpPort,tpStatic
>                               , Window, UI, (#), (#+), (#.), string, on
>                               )
> import System.FilePath
> import Control.Monad
> import Data.IORef

---------------------------------------------------------------------------
-- Path to data dir. (From Threepenny-gui samples/Path.hs)

#if CABAL
> -- using cabal
> import qualified Paths_threepenny_gui (getDataDir)
> 
> getStaticDir :: IO FilePath
> getStaticDir = (</> "wwwroot") `liftM` Paths_threepenny_gui.getDataDir
> 
#else
> -- using GHCi
> 
> getStaticDir :: IO FilePath
> getStaticDir = return "./wwwroot/"
> 
#endif


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
-- Walking trees

> dfsfold :: Ord b => (a -> b -> a) -> a -> Tree b -> a
> dfsfold f z tree@(Tree rs _) = dfsfold_list tree f z rs
>
> dfsfold_list :: Ord b => Tree b -> (a -> b -> a) -> a -> [b] -> a
> dfsfold_list tree f z ns = foldl (dfsfold_node tree f) z ns
>
> dfsfold_node :: Ord b => Tree b -> (a -> b -> a) -> a -> b -> a
> dfsfold_node tree f z n = f (dfsfold_list tree f z cs) n
>   where cs = getChildren tree n

> preorder :: Ord a => Tree a -> [a]
> preorder = dfsfold (\ns n -> n : ns) []

---------------------------------------------------------------------------
-- Showing trees

> instance (Ord a, Show a) => Show (Tree a) where
>   show (Tree rs map) =  "digraph G {\n"
>                      ++ rootNode
>                      ++ foldl (\acc x -> acc ++ (showNode map x)) "" es
>                      ++ showRootEdges map rs
>                      ++ foldl (\acc x -> acc ++ (showOtherEdges map x)) "" es
>                      ++ "}\n"
>     where (_,es) = unzip (Map.toList map)
>
> rootNode :: String
> rootNode = "root [shape=point,color=black]\n"
>
> showNode :: (Ord a, Show a) => (Map a (Node a)) -> Node a -> String 
> showNode map (n@Node{value=v,i=i,children=cs,status=s}) 
>   =  "  " ++ node i ++ " [shape=\"box\", label=\"" ++ show s ++ buggy
>   ++ ":\\l" ++ (escape . show) v ++ "\\l\"]\n"
>   where node i = "node" ++ show i
>         buggy  = if isBuggy map n then " (faulty)" else ""
>         escape []          = []
>         escape ('"' : ss)  = '\\' : '"' : escape ss
>         escape ('\n' : ss) = '\\' : 'l' : escape ss
>         escape (s   : ss)  = s : escape ss

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

> getNodes :: Tree a -> [a]
> getNodes (Tree _ map) = Map.keys map

--------------------------------------------------------------------------
-- Algorithmic debugging

> debugSession :: (Show a, Ord a, Labeled a) 
>              => [(String,String)] -> Tree a -> IO (Tree a)
> debugSession slices tree
>   = do treeRef <- newIORef tree
>        startGUI defaultConfig
>            { tpPort       = Just 10000
>            , tpStatic     = Just "./wwwroot"
>            } (debugSession' slices treeRef)
>        resultTree <- (readIORef treeRef)
>        return resultTree

> class Labeled a where getLabel :: a -> String

MF TODO: from the System.Process documentation: "On Windows, system passes the command to the Windows command interpreter (CMD.EXE or COMMAND.COM), hence Unixy shell tricks will not work."

> debugSession' :: (Show a, Ord a, Labeled a) 
>               => [(String,String)] -> IORef (Tree a) -> Window -> UI ()
> debugSession' sliceDict treeRef window
>   = do return window # UI.set UI.title "Hoed debugging session"
>        UI.addStyleSheet window "debug.css"
>        img <- UI.img 
>        redraw img treeRef
>        tree <- UI.liftIO $ readIORef treeRef
>        let ns = preorder tree
>        ts <- toElems sliceDict ns
>        ds <- mapM (uncurry divpack) (zip ts (cycle [Odd,Even]))
>        buttons <- UI.div #. "buttons" #+ (map UI.element ds)
>        nowrap  <- UI.div #. "nowrap" #+ (map UI.element [buttons,img])
>        UI.getBody window #+ [UI.element nowrap]
>        mapM_ (onClick img treeRef Correct) (zip (corButtons ts) (reverse ns))
>        mapM_ (onClick img treeRef Wrong)   (zip (wrnButtons ts) (reverse ns))

> onClick :: (Show a, Ord a) => UI.Element -> IORef (Tree a) -> Status
>                            -> (UI.Element,a) -> UI ()
> onClick img treeRef status (b,n) 
>   = do on UI.click b $ const $ do updateTree img treeRef 
>                                       (\tree -> markNode tree n status)

ElemSet with representation of the equation and the correct/wrong buttons.

> redraw :: (Show a, Ord a) => UI.Element -> IORef (Tree a) -> UI ()
> redraw img treeRef 
>   = do tree <- UI.liftIO $ readIORef treeRef
>        UI.liftIO $ writeFile "debugTree.dot" (show tree)
>        UI.liftIO $ system $ "dot -Tpng -Gsize=7,8 -Gdpi=100 debugTree.dot "
>                           ++ "> wwwroot/debugTree.png"
>        url <- UI.loadFile "image/png" "wwwroot/debugTree.png"
>        UI.element img # UI.set UI.src url
>        return ()

> updateTree :: (Show a, Ord a) => UI.Element -> IORef (Tree a) 
>                               -> (Tree a -> Tree a) -> UI ()
> updateTree img treeRef f
>   = do tree <- UI.liftIO $ readIORef treeRef
>        UI.liftIO $ writeIORef treeRef (f tree)
>        redraw img treeRef


> --              Slice      Hr         Equation   Correct    Wrong
> type ElemSet = (UI.Element,UI.Element,UI.Element,UI.Element,UI.Element)

 flattenElemSets :: [ElemSet] -> [UI.Element]
 flattenElemSets = fst . (foldl pack ([],1))
   where pack (es,x) set = (divpack set x : es, toggle x)
         toggle 1 = 2
         toggle 2 = 1

> data OddEven = Odd | Even
>
> divpack :: ElemSet -> OddEven -> UI UI.Element
> divpack (e1,e2,e3,e4,e5) x
>   = UI.div #. lbl x #+ map UI.element [e1,e2,e3,e4,e5]
>     where lbl Odd  = "odd"
>           lbl Even = "even"

> corButtons :: [ElemSet] -> [UI.Element]
> corButtons = foldl (\es (_,_,_,e,_) -> e : es) []

> wrnButtons :: [ElemSet] -> [UI.Element]
> wrnButtons = foldl (\es (_,_,_,_,e) -> e : es) []

> toElems :: (Show a, Labeled a) => [(String,String)] -> [a] -> UI [ElemSet]
> toElems sliceDict xs = mapM (toElem sliceDict) xs
>
> toElem :: (Show a, Labeled a) => [(String,String)] -> a -> UI ElemSet
> toElem sliceDict x 
>   = do slc <- UI.pre    # UI.set UI.text (getSlice x)
>        hr  <- UI.hr
>        shw <- UI.pre    # UI.set UI.text (show x)
>        cor <- UI.button # UI.set UI.text "right"
>        wrg <- UI.button # UI.set UI.text "wrong"
>        return (slc,hr,shw,cor,wrg)
>    where getSlice x = case getLabel x `lookup` sliceDict of
>               Nothing -> "??"
>               Just s  -> s


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
