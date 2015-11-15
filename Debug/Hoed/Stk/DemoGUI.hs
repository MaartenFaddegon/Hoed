-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2015

module Debug.Hoed.Stk.DemoGUI
where

import Prelude hiding(Right)
import Debug.Hoed.Stk.Render
import Data.Graph.Libgraph
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny (startGUI,defaultConfig, Window, UI, (#), (#+), (#.), string, on,get,set)
import System.Process(system)
import Data.IORef
import Data.List(intersperse,nub)
import Text.Regex.Posix

--------------------------------------------------------------------------------
-- The tabbed layout from which we select the different views

preorder :: CompGraph -> [Vertex]
preorder = getPreorder . getDfs

showStmt :: UI.Element -> IORef [Vertex] -> IORef Int -> UI ()
showStmt e filteredVerticesRef currentVertexRef = do 
  mv <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
  let s = case mv of
                Nothing  -> "Select vertex above to show details."
                (Just v) -> showCompStmts v
  UI.element e # UI.set UI.text s
  return ()

data Filter = ShowAll | ShowSucc | ShowPred | ShowMatch

demoGUI :: [(String,String)] -> IORef CompGraph -> Window -> UI ()
demoGUI sliceDict treeRef window
  = do return window # UI.set UI.title "Hoed debugging session"
       UI.addStyleSheet window "debug.css"

       -- Get a list of vertices from the computation graph
       tree <- UI.liftIO $ readIORef treeRef
       let ns = filter (not . isRoot) (preorder tree)

       -- Shared memory
       filteredVerticesRef <- UI.liftIO $ newIORef ns
       currentVertexRef    <- UI.liftIO $ newIORef (0 :: Int)
       regexRef            <- UI.liftIO $ newIORef ""
       imgCountRef         <- UI.liftIO $ newIORef (0 :: Int)

       -- Draw the computation graph
       img  <- UI.img 
       redraw img imgCountRef treeRef (Just $ head ns)
       img' <- UI.center #+ [UI.element img]

       -- Field to show computation statement(s) of current vertex
       compStmt <- UI.pre

       -- Menu to select which statement to show
       menu <- UI.select
       showStmt compStmt filteredVerticesRef currentVertexRef
       updateMenu menu treeRef currentVertexRef filteredVerticesRef
       let selectVertex' = selectVertex compStmt filteredVerticesRef currentVertexRef $ redrawWith img imgCountRef treeRef
       on UI.selectionChange menu selectVertex'

       -- Buttons for the various filters
       filterTxt    <- UI.span   # UI.set UI.text "Filters: "
       showAllBut   <- UI.button # UI.set UI.text "Show all"
       showSuccBut  <- UI.button # UI.set UI.text "Show successors"
       showPredBut  <- UI.button # UI.set UI.text "Show predecessors"
       showMatchBut <- UI.button # UI.set UI.text "Matches "
       matchField   <- UI.input
       filters      <- UI.div #+ (map return [ filterTxt, showAllBut, showSuccBut, showPredBut
                                             , showMatchBut, matchField])

       on UI.valueChange matchField $ \s -> UI.liftIO $ writeIORef regexRef s
       let onClickFilter' = onClickFilter menu treeRef currentVertexRef filteredVerticesRef
                                          selectVertex' regexRef
       onClickFilter' showAllBut   ShowAll
       onClickFilter' showSuccBut  ShowSucc
       onClickFilter' showPredBut  ShowPred
       onClickFilter' showMatchBut ShowMatch

       -- Status
       statusSpan <- UI.span
       updateStatus statusSpan treeRef 

       -- Buttons to judge the current statement
       right <- UI.button # UI.set UI.text "right"
       wrong <- UI.button # UI.set UI.text "wrong"
       let onJudge = onClick statusSpan menu img imgCountRef treeRef 
                             currentVertexRef filteredVerticesRef
       onJudge right Right
       onJudge wrong Wrong

       -- Populate the main screen
       hr <- UI.hr
       UI.getBody window #+ (map UI.element [filters, menu, right, wrong, statusSpan
                                            , compStmt, hr,img'])
       return ()

updateMenu :: UI.Element -> IORef CompGraph
              -> IORef Int -> IORef [Vertex] -> UI ()
updateMenu menu treeRef currentVertexRef filteredVerticesRef = do
       g  <- UI.liftIO $ readIORef treeRef
       i  <- UI.liftIO $ readIORef currentVertexRef
       ns <- UI.liftIO $ readIORef filteredVerticesRef
       let fs = faultyVertices g
       ops  <- mapM (\s->UI.option # UI.set UI.text s)
                                $ if ns == [] then ["No matches found"]
                                  else map (summarizeVertex fs) ns
       (UI.element menu) # UI.set UI.children []
       UI.element menu #+ (map UI.element ops)
       (UI.element menu) # UI.set UI.selection (Just i)
       return ()

vertexFilter :: Filter -> CompGraph -> Vertex -> String -> [Vertex]
vertexFilter f g cv r = filter (not . isRoot) $ case f of 
  ShowAll   -> preorder g
  ShowSucc  -> succs g cv
  ShowPred  -> preds g cv
  ShowMatch -> filter matches (preorder g)
    where matches Root = False
          matches v    = showCompStmts v =~ r

onClick :: UI.Element -> UI.Element -> UI.Element 
           -> IORef Int -> IORef CompGraph -> IORef Int -> IORef [Vertex]
           -> UI.Element -> Judgement-> UI ()
onClick statusSpan menu img imgCountRef treeRef currentVertexRef filteredVerticesRef b j = do
  on UI.click b $ \_ -> do
        (Just v) <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
        replaceFilteredVertex v (newStatus v j)
        updateTree img imgCountRef treeRef (Just v) (\tree -> markNode tree v j)
        updateMenu menu treeRef currentVertexRef filteredVerticesRef
        updateStatus statusSpan treeRef

  where replaceFilteredVertex v w = do
          vs <- UI.liftIO $ readIORef filteredVerticesRef
          UI.liftIO $ writeIORef filteredVerticesRef $ map (\x -> if x == v then w else x) vs

newStatus Root _ = Root
newStatus v j    = v{status=j}

lookupCurrentVertex :: IORef Int -> IORef [Vertex] -> IO (Maybe Vertex)
lookupCurrentVertex currentVertexRef filteredVerticesRef = do
  i <- readIORef currentVertexRef
  m <- readIORef filteredVerticesRef
  return $ if i < length m then Just (m !! i) else Nothing

-- onSelectVertex :: UI.Element -> UI.Element -> IORef [Vertex] -> IORef Int 
--                   -> (IORef [Vertex] -> IORef Int -> UI ()) -> UI ()
-- onSelectVertex menu compStmt filteredVerticesRef currentVertexRef myRedraw = do
--   on UI.selectionChange menu $ \mi -> case mi of
--         Just i  -> do UI.liftIO $ writeIORef currentVertexRef i
--                       showStmt compStmt filteredVerticesRef currentVertexRef
--                       myRedraw filteredVerticesRef currentVertexRef 
--                       return ()
--         Nothing -> return ()

selectVertex :: UI.Element -> IORef [Vertex] -> IORef Int  
        -> (IORef [Vertex] -> IORef Int -> UI ()) -> Maybe Int -> UI ()
selectVertex compStmt filteredVerticesRef currentVertexRef myRedraw mi = case mi of
        Just i  -> do UI.liftIO $ writeIORef currentVertexRef i
                      showStmt compStmt filteredVerticesRef currentVertexRef
                      myRedraw filteredVerticesRef currentVertexRef 
                      return ()
        Nothing -> return ()


onClickFilter :: UI.Element -> IORef CompGraph -> IORef Int -> IORef [Vertex] 
                  -> (Maybe Int -> UI ()) -> IORef String -> UI.Element -> Filter -> UI ()
onClickFilter menu treeRef currentVertexRef filteredVerticesRef selectVertex' regexRef e fil = do
  on UI.click e $ \_ -> do
    mcv <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
    g <- UI.liftIO $ readIORef treeRef
    r <- UI.liftIO $ readIORef regexRef
    let cv = case mcv of (Just v) -> v
                         Nothing  -> head . (filter $ not . isRoot) . preorder $ g
        applyFilter f = do UI.liftIO $ writeIORef filteredVerticesRef (vertexFilter f g cv r)
                           UI.liftIO $ writeIORef currentVertexRef 0
    applyFilter fil
    updateMenu menu treeRef currentVertexRef filteredVerticesRef
    selectVertex' (Just 0)

-- MF TODO: We may need to reconsider how Vertex is defined,
-- and how we determine equality. I think it could happen that
-- two vertices with equal equation but different stacks/relations
-- are now both changed.
markNode :: CompGraph -> Vertex -> Judgement -> CompGraph
markNode g v s = mapGraph f g
  where f Root = Root
        f v'   = if v' === v then newStatus v s else v'

        (===) :: Vertex -> Vertex -> Bool
        Root === v = v == Root
        v1 === v2 = (equations v1) == (equations v2)

data MaxStringLength = ShorterThan Int | Unlimited

shorten :: MaxStringLength -> String -> String
shorten Unlimited s = s
shorten (ShorterThan l) s
          | length s < l = s
          | l > 3        = take (l - 3) s ++ "..."
          | otherwise    = take l s

-- MF TODO: Maybe we should do something smart with witespace substitution here?
noNewlines :: String -> String
noNewlines = filter (/= '\n')

showCompStmts :: Vertex -> String
showCompStmts = commas . equations'
  where equations' Root = ["Root"]
        equations' v    = map show . equations $ v
        
summarizeVertex :: [Vertex] -> Vertex -> String
summarizeVertex fs v = shorten (ShorterThan 27) (noNewlines $ showCompStmts v) ++ s
  where s = if v `elem` fs then " !!" else case getStatus v of
              Unassessed     -> " ??"
              Wrong          -> " :("
              Right          -> " :)"

updateStatus :: UI.Element -> IORef CompGraph -> UI ()
updateStatus e compGraphRef = do
  g <- UI.liftIO $ readIORef compGraphRef
  let getLabel Root = "Root"
      getLabel v = commas . (map equLabel) . equations $ v
      isJudged v = getStatus v /= Unassessed
      slen       = show . length
      ns = filter (not . isRoot) (preorder g)
      js = filter isJudged ns
      fs = faultyVertices g
      txt = if length fs > 0 then " Fault detected in: " ++ getLabel (head fs)
                             else " Judged " ++ slen js ++ "/" ++ slen ns
  UI.element e # UI.set UI.text txt
  return ()

updateTree :: UI.Element -> IORef Int -> IORef CompGraph -> (Maybe Vertex) -> (CompGraph -> CompGraph)
           -> UI ()
updateTree img imgCountRef treeRef mcv f
  = do tree <- UI.liftIO $ readIORef treeRef
       UI.liftIO $ writeIORef treeRef (f tree)
       redraw img imgCountRef treeRef mcv

redrawWith :: UI.Element -> IORef Int -> IORef CompGraph -> IORef [Vertex] -> IORef Int -> UI ()
redrawWith img imgCountRef treeRef filteredVerticesRef currentVertexRef = do
  mv <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
  redraw img imgCountRef treeRef mv

redraw :: UI.Element -> IORef Int -> IORef CompGraph -> (Maybe Vertex) -> UI ()
redraw img imgCountRef treeRef mcv
  = do tree <- UI.liftIO $ readIORef treeRef
       -- replace with the following line to "summarize" big trees  by
       -- dropping some nodes
       --UI.liftIO $ writeFile ".Hoed/debugTree.dot" (shw $ summarize tree mcv)
       UI.liftIO $ writeFile ".Hoed/debugTree.dot" (shw tree)
       UI.liftIO $ system $ "dot -Tpng -Gsize=9,5 -Gdpi=100 .Hoed/debugTree.dot "
                          ++ "> .Hoed/wwwroot/debugTree.png"
       i <- UI.liftIO $ readIORef imgCountRef
       UI.liftIO $ writeIORef imgCountRef (i+1)
       -- Attach counter to image url to reload image
       UI.element img # set UI.src ("static/debugTree.png#" ++ show i)
       return ()

  where shw g = showWith g (coloVertex $ faultyVertices g) showArc
        coloVertex _ Root = ("\".\"", "shape=none")
        coloVertex fs v = ( "\"" ++ escape (summarizeVertex fs v) ++ "\""
                          , if isCurrentVertex mcv v then "style=filled fillcolor=yellow"
                                                     else ""
                          )
        showArc _  = ""

-- MF TODO: summarize now just "throws away" some vertices if there are too
-- many. Better would be to inject summarizing nodes (e.g. "and 25 more").
summarize :: CompGraph -> Maybe Vertex -> CompGraph
summarize g mcv = Graph (root g) keep as
  where keep1 v = take 7 $ succs g v
        keep'   = nub $ Root : foldl (\ks v -> ks ++ keep1 v) [] (vertices g)
        keep    = case filter (isCurrentVertex mcv) (vertices g) of
                        []    -> keep'
                        (w:_) -> if w `elem` keep' then keep' else w : keep
        as      = filter (\(Arc v w _) -> v `elem` keep && w `elem` keep) (arcs g)

isCurrentVertex :: Maybe Vertex -> Vertex -> Bool
isCurrentVertex mcv v = case v of
  Root -> False
  _    -> case mcv of 
                Nothing     -> False
                (Just Root) -> False
                (Just w)    -> equations v == equations w

commas :: [String] -> String
commas []  = error "commas: empty list"
commas [e] = e
commas es  = foldl (\acc e-> acc ++ e ++ ", ") "{" (init es) ++ (last es) ++ "}"

faultyVertices :: CompGraph -> [Vertex]
faultyVertices = findFaulty_dag getStatus

getStatus Root = Right
getStatus v    = status v
