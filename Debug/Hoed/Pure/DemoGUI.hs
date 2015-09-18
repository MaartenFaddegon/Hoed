-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2015
{-# LANGUAGE CPP #-}

module Debug.Hoed.Pure.DemoGUI
where

import Prelude hiding(Right)
import Debug.Hoed.Pure.Render
import Debug.Hoed.Pure.CompTree
import Debug.Hoed.Pure.EventForest
import Debug.Hoed.Pure.Observe
import Data.Graph.Libgraph
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny (startGUI,defaultConfig, Window, UI, (#), (#+), (#.), string, on,get,set)
import System.Process(system)
import Data.IORef
import Text.Regex.Posix
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List(intersperse,nub,sort,sortBy
#if __GLASGOW_HASKELL__ >= 710
                , sortOn
#endif
                )

#if __GLASGOW_HASKELL__ < 710
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  = map snd . sortOn' fst .  map (\x -> (f x, x))

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f = sortBy (\x y -> compare (f x) (f y))
#endif

--------------------------------------------------------------------------------
-- The tabbed layout from which we select the different views

guiMain :: Trace -> TraceInfo -> IORef CompTree ->  EventForest -> Window -> UI ()
guiMain trace traceInfo treeRef frt window
  = do return window # set UI.title "Hoed debugging session"

       -- Get a list of vertices from the computation graph
       tree <- UI.liftIO $ readIORef treeRef
       let ns = filter (not . isRootVertex) (preorder tree)

       -- Shared memory
       filteredVerticesRef <- UI.liftIO $ newIORef ns
       currentVertexRef    <- UI.liftIO $ newIORef (vertexUID . head $ ns)
       regexRef            <- UI.liftIO $ newIORef ""
       imgCountRef         <- UI.liftIO $ newIORef (0 :: Int)

       -- Tabs to select which pane to display
       tab1 <- UI.button # set UI.text "About"                 # set UI.style activeTab
       tab2 <- UI.button # set UI.text "Observe"               # set UI.style otherTab
       tab3 <- UI.button # set UI.text "Explore"               # set UI.style otherTab
       tab4 <- UI.button # set UI.text "Algorithmic Debugging" # set UI.style otherTab
       tab5 <- UI.button # set UI.text "Events"                # set UI.style otherTab
       tabs <- UI.div    # set UI.style [("background-color","#D3D3D3")]  #+ (map return [tab1,tab2,tab3,tab4,tab5])

       let coloActive tab = do mapM_ (\t -> t # set UI.style otherTab) [return tab1,return tab2,return tab3,return tab4,return tab5]; return tab # set UI.style activeTab

       help <- guiHelp # set UI.style [("margin-top","0.5em")]
       on UI.click tab1 $ \_ -> do
            coloActive tab1
            UI.getBody window # set UI.children [tabs,help]
       on UI.click tab2 $ \_ -> do
            coloActive tab2
            pane <- guiObserve treeRef currentVertexRef # set UI.style [("margin-top","0.5em")]
            UI.getBody window # set UI.children [tabs,pane]
       on UI.click tab3 $ \_ -> do
            coloActive tab3
            pane <- guiExplore treeRef filteredVerticesRef currentVertexRef regexRef imgCountRef # set UI.style [("margin-top","0.5em")]
            UI.getBody window # set UI.children [tabs,pane]
       on UI.click tab4 $ \_ -> do
            coloActive tab4
            pane <- guiAlgoDebug treeRef filteredVerticesRef currentVertexRef regexRef imgCountRef # set UI.style [("margin-top","0.5em")]
            UI.getBody window # set UI.children [tabs,pane]
       on UI.click tab5 $ \_ -> do
         coloActive tab5
         pane <- guiTrace trace traceInfo # set UI.style [("margin-top","0.5em")]
         UI.getBody window # set UI.children [tabs,pane]

       UI.getBody window # set UI.style [("margin","0")] #+ (map return [tabs,help])
       return ()


activeTab = ("background-color", "white") : tabstyle
otherTab  = ("background-color", "#f0f0f0") : tabstyle
tabstyle = [("-webkit-border-top-left-radius", "19"), ("-moz-border-top-left-radius", "19"), ("border-top-left-radius", "19px"),("-webkit-border-top-right-radius", "19"), ("-moz-border-top-right-radius", "19"), ("border-top-right-radius", "19px"), ("border-width", "medium medium 0px")]

--------------------------------------------------------------------------------
-- The help/welcome page

guiHelp :: UI UI.Element
guiHelp = UI.div # set UI.style [("margin-left", "20%"),("margin-right", "20%")] #+ 
  [ UI.h1 # set UI.text "Welcome to Hoed"
  , UI.p # set UI.text "Hoed is a tracer and debugger for the language Haskell. You can trace a program by annotating functions in suspected modules. After running the program the trace can be viewed in different ways using a web browser. Use the tabs at the top of this page to select the view you want to use. Below we give a short explenation of each view."
  , UI.h2 # set UI.text "Observe"
  , UI.p # set UI.text "The observe view is useful to get a first impression of what is happening in your program, or to get an overview of the computation statements of a particular slice or pattern. At the top the list of slices and for each slice how many times it was reduced. Below the line a list of computation statements."
  , UI.h2 # set UI.text "Algorithmic Debugging"
  , UI.p # set UI.text "The algorithmic debugger shows you recorded computation statements, that is a function applied to an argument and its result. You judge these statements as right or wrong. When enough statements are judged the debugger tells you the location of the fault in your code."
  , UI.h2 # set UI.text "Explore"
  , UI.p # set UI.text "The trace is translated into a tree of computation statements for the algorithmic debugging view. In the explore view you can freely browse this tree to get a better understanding of your program. You can decide yourself in which order you want to judge statements. When enough statements are judged the debugger tells you the location of the fault in your code."
  ] 


--------------------------------------------------------------------------------
-- The page showing the list of events in the trace

guiTrace :: Trace -> TraceInfo -> UI UI.Element
guiTrace trace traceInfo = do
  rows <- mapM (\e -> eventRow e traceInfo) (reverse trace)
  tbl  <- UI.table #+ map return rows
  UI.span #+ [return tbl]

eventRow :: Event -> TraceInfo -> UI UI.Element
eventRow e traceInfo = do
  sign <- UI.td # set UI.text (let sym = case getLocation e traceInfo of True  -> "* "; False -> "o "
                               in case change e of
                                    Observe{} -> " "
                                    Fun{}     -> " "
                                    Enter{}   -> sym
                                    Cons{}    -> sym)
  evnt <- UI.td # set UI.text (show e)
  msg <- UI.td # set UI.text (getMessage e traceInfo)
  UI.tr #+ map return [sign,evnt,msg]

getStk :: Event -> TraceInfo -> String
getStk e traceInfo = case change e of
  Enter{}  -> case IntMap.lookup (eventUID e) (storedStack traceInfo) of 
              (Just stk) -> show stk
              Nothing    -> ""
  _        -> ""


--------------------------------------------------------------------------------
-- The observe GUI

guiObserve :: IORef CompTree -> IORef Int -> UI UI.Element
guiObserve treeRef currentVertexRef = do
       (Graph _ vs _) <- UI.liftIO $ readIORef treeRef

       -- Alphabetical sorted list of slices, and for each slice how many computation statements
       -- there are for that slice
       let slices' = sort $ map (stmtLabel . vertexStmt) . filter (not . isRootVertex) $ vs
           slices  = nub slices'
           count slice = length (filter (==slice) slices')
           span s = UI.span # set UI.text s # set UI.style [("margin-right","1em")]
           spans = map (\(c,lbl) -> span $ show c ++ " " ++ lbl) $ zip (map count slices) slices

       -- Alphabetical sorted list of computation statements
       let vs_sorted = sortOn (stmtRes . vertexStmt) . filter (not . isRootVertex) $ vs
       stmtDiv <- UI.form # set UI.style [("margin-left","2em")]
       updateRegEx currentVertexRef vs_sorted stmtDiv "" -- with empty regex to fill div3 1st time

       -- The regexp filter
       matchField  <- UI.input
       on UI.valueChange matchField (updateRegEx currentVertexRef vs_sorted stmtDiv)

       UI.div  #+ (spans ++ [UI.br, UI.span # set UI.text "regex filter: ", return matchField, UI.hr, return stmtDiv])

updateRegEx :: IORef Int -> [Vertex] -> UI.Element -> String -> UI ()
updateRegEx currentVertexRef vs stmtDiv r = draw

  where draw = do (return stmtDiv) # set UI.children [] #+ csDivs; return ()

        vs_filtered = if r == "" then vs else filter (\v -> (stmtRes . vertexStmt) v =~ r) vs
        ss = map (stmtRes . vertexStmt) vs_filtered

        csDivs = map stmtToDiv vs_filtered

        stmtToDiv v = do
          i <- UI.liftIO $ readIORef currentVertexRef
          s <- UI.span # set UI.text (stmtRes . vertexStmt $ v)
          r <- UI.input # set UI.type_ "radio" # set UI.checked (i == vertexUID v)
          on UI.checkedChange r $ \_ -> checked v
          UI.div #+ [return r, return s]

        -- MF TODO: instead of re-drawing it would be prettier to just
        -- read out the old value of currentVertexRef and set the appropriate
        -- radio button's checked property to False.
        checked v = do
          UI.liftIO $ writeIORef currentVertexRef (vertexUID v)
          draw

--------------------------------------------------------------------------------
-- The Algorithmic Debugging GUI

guiAlgoDebug :: IORef CompTree -> IORef [Vertex] -> IORef Int -> IORef String -> IORef Int -> UI UI.Element
guiAlgoDebug treeRef filteredVerticesRef currentVertexRef regexRef imgCountRef = do

       -- Get a list of vertices from the computation graph
       tree <- UI.liftIO $ readIORef treeRef
       let ns = filter (not . isRootVertex) (preorder tree)

       -- Status
       status <- UI.span
       updateStatus status treeRef 

       -- Field to show computation statement(s) of current vertex
       compStmt <- UI.pre
       showStmt compStmt filteredVerticesRef currentVertexRef

       -- Buttons to judge the current statement
       right <- UI.button # UI.set UI.text "right " #+ [UI.img # set UI.src "static/right.png" # set UI.height 30]
       wrong <- UI.button # set UI.text "wrong "    #+ [UI.img # set UI.src "static/wrong.png" # set UI.height 30]
       judge status compStmt right Right
       judge status compStmt wrong Wrong

       -- Populate the main screen
       let top = compStmt
       -- top    <- UI.center #+ [return compStmt]
       bottom <- UI.center #+ [return right, return wrong, UI.br, return status]
       UI.div #+ [return top, UI.hr, return bottom]

       where 
       judge status compStmt b j = 
         on UI.click b $ \_ -> do
           (Just v) <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
           tree'    <- UI.liftIO $ readIORef treeRef
           let tree  = markNode tree' v j
               w     = next_step tree vertexJmt v{vertexJmt=j}
           UI.element compStmt # UI.set UI.text (show . vertexStmt $ w)
           UI.liftIO $ writeIORef currentVertexRef (vertexUID w)
           UI.liftIO $ writeIORef treeRef tree
           vs <- UI.liftIO $ readIORef filteredVerticesRef
           updateStatus status treeRef 
           UI.liftIO $ writeIORef filteredVerticesRef $ map (\x -> if x == v then v{vertexJmt=j} else x) vs

--------------------------------------------------------------------------------
-- Explore the computation tree

guiExplore :: IORef CompTree -> IORef [Vertex] -> IORef Int -> IORef String -> IORef Int -> UI UI.Element
guiExplore treeRef filteredVerticesRef currentVertexRef regexRef imgCountRef = do

       -- Get a list of vertices from the computation graph
       tree <- UI.liftIO $ readIORef treeRef
       let ns = filter (not . isRootVertex) (preorder tree)

       -- Draw the computation graph
       img  <- UI.img 
       redrawWith img imgCountRef treeRef filteredVerticesRef currentVertexRef
       img' <- UI.center #+ [UI.element img]

       -- Field to show computation statement(s) of current vertex
       compStmt <- UI.pre

       -- Menu to select which statement to show
       menu <- UI.select
       showStmt compStmt filteredVerticesRef currentVertexRef
       updateMenu menu treeRef currentVertexRef filteredVerticesRef
       let selectVertex' = selectVertex compStmt filteredVerticesRef currentVertexRef (redrawWith img imgCountRef treeRef)
       on UI.selectionChange menu selectVertex'

       -- Buttons for the various filters
       filterTxt    <- UI.span   # set UI.text "Filters: "
       showAllBut   <- UI.button # set UI.text "Show all"
       showSuccBut  <- UI.button # set UI.text "Show successors"
       showPredBut  <- UI.button # set UI.text "Show predecessors"
       showMatchBut <- UI.button # set UI.text "Matches "
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
       status <- UI.span
       updateStatus status treeRef 

       -- Buttons to judge the current statement
       right <- UI.button # UI.set UI.text "right " #+ [UI.img # set UI.src "static/right.png" # set UI.height 20]
       wrong <- UI.button # set UI.text "wrong "    #+ [UI.img # set UI.src "static/wrong.png" # set UI.height 20]
       let onJudge = onClick status menu img imgCountRef treeRef currentVertexRef filteredVerticesRef
       onJudge right Right
       onJudge wrong Wrong

       -- Populate the main screen
       hr <- UI.hr
       UI.div #+ (map UI.element [filters, menu, right, wrong, status, compStmt, hr,img'])


preorder :: CompTree -> [Vertex]
preorder = getPreorder . getDfs

showStmt :: UI.Element -> IORef [Vertex] -> IORef Int -> UI ()
showStmt e filteredVerticesRef currentVertexRef = do 
  mv <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
  let s = case mv of
                Nothing  -> "Select vertex above to show details."
                (Just v) -> show . vertexStmt $ v
  UI.element e # set UI.text s
  return ()

data Filter = ShowAll | ShowSucc | ShowPred | ShowMatch

updateMenu :: UI.Element -> IORef CompTree
              -> IORef Int -> IORef [Vertex] -> UI ()
updateMenu menu treeRef currentVertexRef filteredVerticesRef = do
       g  <- UI.liftIO $ readIORef treeRef
       vs <- UI.liftIO $ readIORef filteredVerticesRef
       i  <- UI.liftIO $ readIORef currentVertexRef
       let j = pos (\v -> vertexUID v == i) vs
       let fs = faultyVertices g
       ops  <- mapM (\s->UI.option # set UI.text s)
                                $ if vs == [] then ["No matches found"]
                                  else map (summarizeVertex fs) vs
       (UI.element menu) # set UI.children []
       UI.element menu #+ (map UI.element ops)
       (UI.element menu) # set UI.selection (Just j)
       return ()

pos :: (a->Bool) -> [a] -> Int
pos p xs = case filter (\(_,x) -> p x) $ zip [0..] xs of 
                []        -> -1
                ((i,_):_) -> i

vertexFilter :: Filter -> CompTree -> Vertex -> String -> [Vertex]
vertexFilter f g cv r = filter (not . isRootVertex) $ case f of 
  ShowAll   -> preorder g
  ShowSucc  -> succs g cv
  ShowPred  -> preds g cv
  ShowMatch -> filter matches (preorder g)
    where matches RootVertex = False
          matches v    = show v =~ r

onClick :: UI.Element -> UI.Element -> UI.Element 
           -> IORef Int-> IORef CompTree -> IORef Int -> IORef [Vertex]
           -> UI.Element -> Judgement-> UI ()
onClick status menu img imgCountRef treeRef currentVertexRef filteredVerticesRef b j = do
  on UI.click b $ \_ -> do
        (Just v) <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
        replaceFilteredVertex v (v{vertexJmt=j})
        updateTree img imgCountRef treeRef (Just v) (\tree -> markNode tree v j)
        updateMenu menu treeRef currentVertexRef filteredVerticesRef
        updateStatus status treeRef

  where replaceFilteredVertex v w = do
          vs <- UI.liftIO $ readIORef filteredVerticesRef
          UI.liftIO $ writeIORef filteredVerticesRef $ map (\x -> if x == v then w else x) vs

lookupCurrentVertex :: IORef Int -> IORef [Vertex] -> IO (Maybe Vertex)
lookupCurrentVertex currentVertexRef filteredVerticesRef = do
  i  <- readIORef currentVertexRef
  vs <- readIORef filteredVerticesRef
  return $ case filter (\v->vertexUID v==i) vs of
                 []  -> Nothing
                 [v] -> Just v
                 vs   -> error $ "lookupCurrentVertex: UID " ++ show i ++ " identifies "
                                 ++ (show . length $ vs) ++ " computation statements"

selectVertex :: UI.Element -> IORef [Vertex] -> IORef Int  
        -> (IORef [Vertex] -> IORef Int -> UI ()) -> Maybe Int -> UI ()
selectVertex compStmt filteredVerticesRef currentVertexRef myRedraw mi = case mi of
        Just i  -> do vs <- UI.liftIO $ readIORef filteredVerticesRef
                      let v = vs !! i
                      UI.liftIO $ writeIORef currentVertexRef (vertexUID v)
                      showStmt compStmt filteredVerticesRef currentVertexRef
                      myRedraw filteredVerticesRef currentVertexRef 
                      return ()
        Nothing -> return ()


onClickFilter :: UI.Element -> IORef CompTree -> IORef Int -> IORef [Vertex] 
                  -> (Maybe Int -> UI ()) -> IORef String -> UI.Element -> Filter -> UI ()
onClickFilter menu treeRef currentVertexRef filteredVerticesRef selectVertex' regexRef e fil = do
  on UI.click e $ \_ -> do
    mcv <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
    g <- UI.liftIO $ readIORef treeRef
    r <- UI.liftIO $ readIORef regexRef
    let cv = case mcv of (Just v) -> v
                         Nothing  -> head . (filter $ not . isRootVertex) . preorder $ g
        applyFilter f = do UI.liftIO $ writeIORef filteredVerticesRef (vertexFilter f g cv r)
                           UI.liftIO $ writeIORef currentVertexRef 0
    applyFilter fil
    updateMenu menu treeRef currentVertexRef filteredVerticesRef
    selectVertex' (Just 0)

-- MF TODO: We may need to reconsider how Vertex is defined,
-- and how we determine equality. I think it could happen that
-- two vertices with equal equation but different stacks/relations
-- are now both changed.
markNode :: CompTree -> Vertex -> Judgement -> CompTree
markNode g v s = mapGraph f g
  where f RootVertex = RootVertex
        f v'         = if v' === v then v{vertexJmt=s} else v'

        (===) :: Vertex -> Vertex -> Bool
        v1 === v2 = (vertexStmt v1) == (vertexStmt v2)

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

summarizeVertex :: [Vertex] -> Vertex -> String
-- summarizeVertex fs v = shorten (ShorterThan 27) (noNewlines . show . vertexStmt $ v) ++ s
summarizeVertex fs v = (noNewlines . show . vertexStmt $ v) ++ s
  where s = if v `elem` fs then " !!" else case vertexJmt v of
              Wrong          -> " :("
              Right          -> " :)"
              _              -> " ??"
              

updateStatus :: UI.Element -> IORef CompTree -> UI ()
updateStatus e compGraphRef = do
  g <- UI.liftIO $ readIORef compGraphRef
  let getLabel   = stmtLabel . vertexStmt
      isJudged v = vertexJmt v /= Unassessed
      slen       = show . length
      ns = filter (not . isRootVertex) (preorder g)
      js = filter isJudged ns
      fs = faultyVertices g
      txt = if length fs > 0 then " Fault detected in: " -- ++ getLabel (head fs)
                                                         ++ (stmtRes . vertexStmt . head) fs
                             else " Judged " ++ slen js ++ "/" ++ slen ns
  UI.element e # set UI.text txt
  return ()

updateTree :: UI.Element -> IORef Int -> IORef CompTree -> (Maybe Vertex) -> (CompTree -> CompTree)
           -> UI ()
updateTree img imgCountRef treeRef mcv f
  = do tree <- UI.liftIO $ readIORef treeRef
       UI.liftIO $ writeIORef treeRef (f tree)
       redraw img imgCountRef treeRef mcv

redrawWith :: UI.Element -> IORef Int -> IORef CompTree -> IORef [Vertex] -> IORef Int -> UI ()
redrawWith img imgCountRef treeRef filteredVerticesRef currentVertexRef = do
  mv <- UI.liftIO $ lookupCurrentVertex currentVertexRef filteredVerticesRef
  redraw img imgCountRef treeRef mv

redraw :: UI.Element -> IORef Int -> IORef CompTree -> (Maybe Vertex) -> UI ()
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
        coloVertex fs v = ( "\"" ++ (show . vertexUID) v ++ ": "
                            ++ summarizeVertex fs v ++ "\""
                          , if isCurrentVertex mcv v then "style=filled fillcolor=yellow"
                                                     else ""
                          )
        showArc _  = ""

-- MF TODO: summarize now just "throws away" some vertices if there are too
-- many. Better would be to inject summarizing nodes (e.g. "and 25 more").
{-
summarize :: CompTree -> Maybe Vertex -> CompTree
summarize g mcv = Graph (root g) keep as
  where keep1 v = take 7 $ succs g v
        keep'   = nub $ RootVertex : foldl (\ks v -> ks ++ keep1 v) [] (vertices g)
        keep    = case filter (isCurrentVertex mcv) (vertices g) of
                        []    -> keep'
                        (w:_) -> if w `elem` keep' then keep' else w : keep
        as      = filter (\(Arc v w _) -> v `elem` keep && w `elem` keep) (arcs g)
-}

isCurrentVertex :: Maybe Vertex -> Vertex -> Bool
isCurrentVertex mcv v = case v of
  RootVertex -> False
  _    -> case mcv of 
                Nothing           -> False
                (Just RootVertex) -> False
                (Just w)          -> vertexStmt v == vertexStmt w

faultyVertices :: CompTree -> [Vertex]
faultyVertices = findFaulty_dag getStatus
  where getStatus RootVertex = Right
        getStatus v    = vertexJmt v


--------------------------------------------------------------------------------
-- The data flow GUI
{-
guiDDT :: ConstantTree -> IORef Int -> EventForest -> IORef Int -> IORef CompTree -> UI UI.Element
guiDDT ddt imgCountRef frt currentVertexRef treeRef = do
  (Graph _ vs _) <- UI.liftIO $ readIORef treeRef
  let idStmts = map (\(Vertex s _) -> (equIdentifier s, s)) . filter (not . isRootVertex) $ vs

  -- The current computation statement
  i  <- UI.liftIO $ readIORef currentVertexRef
  stmtDiv <- UI.div # set UI.text (case lookup i idStmts of 
        Nothing  -> "?"
        (Just s) -> equRes s)

  -- Menu to select constant (floating on the right of the page) 
  let (Graph _ cs _) = ddt
      cs_i = filter (\c -> case c of CVRoot -> False; _ -> valStmt c == i) cs
  cSel   <- UI.select #+ map (\c -> UI.option # set UI.text (shwConst c)) cs_i
  shwBut <- UI.button # set UI.text "Show me"
  selDiv <- UI.div # set UI.style [("float","right")] #+ [return shwBut, UI.span # set UI.text " where ", return cSel, UI.span # set UI.text " comes from."]

  -- The dynamic data dependency tree
  img <- UI.img
  drawDDT ddt img imgCountRef frt
  centImg <- UI.center #+ [UI.element img]

  UI.div #+ [return selDiv, return stmtDiv, UI.hr, return centImg]

  where shwConst c = shwLoc c ++ "\"" ++ (case lookup (valMax c) es of (Just e) -> render frt e; Nothing -> "?") ++ "\""
        shwLoc c = case (show . valLoc) c of "" -> ""; s -> s ++ ": "
        es = eventsByUID frt

updateStmt :: UI.Element -> IORef Int -> [CompStmt] -> Int -> UI ()
updateStmt stmtDiv currentVertexRef ss pos = do
  let s = ss !! pos
  return stmtDiv # set UI.text (equRes s)
  UI.liftIO $ writeIORef currentVertexRef (equIdentifier s)

drawDDT :: ConstantTree -> UI.Element -> IORef Int -> EventForest -> UI ()
drawDDT ddt img imgCountRef frt = do
  UI.liftIO $ writeFile ".Hoed/dynData.dot" (shw ddt)
  UI.liftIO $ system $ "dot -Tpng -Gsize=9,9 -Gdpi=100 .Hoed/dynData.dot > .Hoed/wwwroot/dynData.png"
  i <- UI.liftIO $ readIORef imgCountRef
  UI.liftIO $ writeIORef imgCountRef (i+1)
  UI.element img # set UI.src ("static/dynData.png#" ++ show i)
  return ()

  where shw g = showWith g shwConst shwArc
        shwConst c = ("Stmt-"++ (show . valStmt) c ++ "-" ++ (show . valLoc) c ++ ": " ++ case lookup (valMax c) es of (Just e) -> render frt e; Nothing -> "?","")
        shwArc _   = ""
        es = eventsByUID frt

eventsByUID :: EventForest -> [(UID,Event)]
-- eventsByUID = map (\e -> (eventUID e, e)) . foldl (\acc1 (_,es_p) -> foldl (\acc2 (_,e) -> e : acc2) acc1 es_p) []
eventsByUID = map (\e -> (eventUID e, e)) . map snd . concat . elems

render :: EventForest -> Event -> String
render frt r = dfsFold Infix pre post "" Trunk (Just r) frt
  where pre Nothing  _ = (++" _")
        pre (Just e) _ = case change e of
          (Observe s _ _ _) -> (++s)
          (Cons _ s)        -> (++" ("++s)
          Enter             -> id
          NoEnter           -> id
          Fun               -> (++"{\\")
        post Nothing  _ = id
        post (Just e) _ = case change e of
          (Observe s _ _ _) -> id
          (Cons _ s)        -> (++")")
          Enter             -> id
          NoEnter           -> id
          Fun               -> (++"}")

-}
