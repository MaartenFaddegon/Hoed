-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014

module Debug.Hoed.DemoGUI
where

import Prelude hiding(Right)
import Debug.Hoed.Render
import Data.Graph.Libgraph
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny (startGUI,defaultConfig,tpPort,tpStatic
                              , Window, UI, (#), (#+), (#.), string, on
                              )
import System.Process(system)
import Data.IORef



preorder :: CompGraph -> [Vertex]
preorder = getPreorder . getDfs

demoGUI :: [(String,String)] -> IORef CompGraph -> Window -> UI ()
demoGUI sliceDict treeRef window
  = do return window # UI.set UI.title "Hoed debugging session"
       UI.addStyleSheet window "debug.css"
       img <- UI.img 
       redraw img treeRef
       buttons <- UI.div #. "buttons"
       nowrap  <- UI.div #. "nowrap"  #+ (map UI.element [buttons,img])
       UI.getBody window #+ (map UI.element [nowrap])

       tree <- UI.liftIO $ readIORef treeRef
       let ns = (preorder tree)
       ts <- toElems sliceDict ns
       ds <- mapM (uncurry divpack) (zip ts (cycle [Odd,Even]))
       UI.element buttons # UI.set UI.children ds
       mapM_ (onClick buttons img treeRef Right) 
             (zip (corButtons ts) (reverse ns))
       mapM_ (onClick buttons img treeRef Wrong)
             (zip (wrnButtons ts) (reverse ns))


--              Slice      Hr         CompStmt   Right    Wrong
type ElemSet = (UI.Element,UI.Element,UI.Element,UI.Element,UI.Element)

data OddEven = Odd | Even

divpack :: ElemSet -> OddEven -> UI UI.Element
divpack (e1,e2,e3,e4,e5) x
  = UI.div #. lbl x #+ map UI.element [e1,e2,e3,e4,e5]
    where lbl Odd  = "odd"
          lbl Even = "even"

onClick :: UI.Element -> UI.Element -> IORef CompGraph -> Judgement
        -> (UI.Element,Vertex) -> UI ()
onClick buttons img treeRef status (b,n) 
  = do on UI.click b $ \_ -> do 
        updateTree img treeRef (\tree -> markNode tree n status)
        -- UI.element b # UI.set UI.text "I have been clicked!"
        -- UI.element buttons # UI.set UI.children []
        

-- MF TODO: We may need to reconsider how Vertex is defined,
-- and how we determine equality. I think it could happen that
-- two vertices with equal equation but different stacks/relations
-- are now both changed.
markNode :: CompGraph -> Vertex -> Judgement -> CompGraph
markNode g v s = mapGraph (\v' -> if v' === v then v{status=s} else v') g
  where (===) :: Vertex -> Vertex -> Bool
        v1 === v2 = (equations v1) == (equations v2)

corButtons :: [ElemSet] -> [UI.Element]
corButtons = foldl (\es (_,_,_,e,_) -> e : es) []

wrnButtons :: [ElemSet] -> [UI.Element]
wrnButtons = foldl (\es (_,_,_,_,e) -> e : es) []

toElems :: [(String,String)] -> [Vertex] -> UI [ElemSet]
toElems sliceDict xs = mapM (toElem sliceDict) xs

toElem :: [(String,String)] -> Vertex -> UI ElemSet
toElem sliceDict v 
  = do slc <- UI.pre    # UI.set UI.text (foldl (\acc e -> acc ++ getSlice e) "" $ equations v)
       hr  <- UI.hr
       shw <- UI.pre    # UI.set UI.text (foldl (\acc e -> acc ++ show e ++ "\n") "" $ equations v)
       cor <- UI.button # UI.set UI.text "right"
       wrg <- UI.button # UI.set UI.text "wrong"
       return (slc,hr,shw,cor,wrg)
   where getSlice e = case equLabel e `lookup` sliceDict of
              Nothing -> ""
              Just s  -> s

updateTree :: UI.Element -> IORef CompGraph -> (CompGraph -> CompGraph)
           -> UI ()
updateTree img treeRef f
  = do tree <- UI.liftIO $ readIORef treeRef
       UI.liftIO $ writeIORef treeRef (f tree)
       redraw img treeRef

redraw :: UI.Element -> IORef CompGraph -> UI ()
redraw img treeRef 
  = do tree <- UI.liftIO $ readIORef treeRef
       UI.liftIO $ writeFile "debugTree.dot" (shw tree)
       UI.liftIO $ system $ "dot -Tpng -Gsize=7,8 -Gdpi=100 debugTree.dot "
                          ++ "> wwwroot/debugTree.png"
       url <- UI.loadFile "image/png" "wwwroot/debugTree.png"
       UI.element img # UI.set UI.src url
       return ()

  where shw g = showWith g (showVertex $ faultyVertices g) showArc
        showVertex :: [Vertex] -> Vertex -> String
        showVertex fs v = showStatus fs v ++ ":\n" ++ showCompStmts v
                          ++ "\nwith stack " ++ (show . equStack . head . equations $ v)

        showStatus fs v
          | v `elem` fs = "Faulty"
          | otherwise   = (show . status) v

        showCompStmts = showCompStmts' . equations
        showCompStmts' [e] = show e
        showCompStmts' es  = foldl (\acc e-> acc ++ show e ++ ", ") "{" (init es) 
                             ++ show (last es) ++ "}"

        -- showVertex = show
        -- showVertex = (foldl (++) "") . (map show) . equations
        showArc _  = ""

faultyVertices :: CompGraph -> [Vertex]
faultyVertices = findFaulty_dag status



