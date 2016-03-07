-- This file is part of the Haskell debugger Hoed.
--
-- Copyright (c) Maarten Faddegon, 2014-2015
{-# LANGUAGE CPP #-}

module Debug.Hoed.Pure.DemoGUI (guiMain,treeFilePath)
where

import qualified Prelude
import Prelude hiding(Right)
import Debug.Hoed.Pure.Render
import Debug.Hoed.Pure.CompTree
import Debug.Hoed.Pure.EventForest
import Debug.Hoed.Pure.Observe
import Debug.Hoed.Pure.Serialize
import qualified Debug.Hoed.Pure.Prop as Prop
import Debug.Hoed.Pure.Prop(Propositions,lookupPropositions,UnevalHandler(..),Judge(..),treeFilePath)
import Paths_Hoed (version)
import Data.Version (showVersion)
import Data.Graph.Libgraph
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny (startGUI,defaultConfig, Window, UI, (#), (#+), (#.), string, on,get,set)
import System.Process(system)
import Data.IORef
import Text.Regex.Posix
import Text.Regex.Posix.String
import Data.Time.Clock
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List(findIndex,intersperse,nub,sort,sortBy
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

guiMain :: Trace -> CompTree -> EventForest -> [Propositions] -> Window -> UI ()
guiMain trace tree frt propositions window
  = do return window # set UI.title "Hoed debugging session"

       -- memory shared between the different views
       compTreeRef <- UI.liftIO $ newIORef tree
       traceRef    <- UI.liftIO $ newIORef trace

       -- Get a list of vertices from the computation graph
       let ns = filter (not . isRootVertex) (preorder tree)

       -- Shared memory
       currentVertexRef    <- UI.liftIO $ newIORef (vertexUID . head $ ns)
       regexRef            <- UI.liftIO $ newIORef ""
       imgCountRef         <- UI.liftIO $ newIORef (0 :: Int)

       -- Tabs to select which pane to display
       tab1 <- UI.button # set UI.text "About"                 # set UI.style activeTab
       tab2 <- UI.button # set UI.text "Observe"               # set UI.style otherTab
       tab3 <- UI.button # set UI.text "Algorithmic Debugging" # set UI.style otherTab
       tab4 <- UI.button # set UI.text "Assisted Debugging"    # set UI.style otherTab
       tab5 <- UI.button # set UI.text "Explore"               # set UI.style otherTab
       tab6 <- UI.button # set UI.text "Stats"                 # set UI.style otherTab
       logo <- UI.img # set UI.src "static/hoed-logo.png"      # set UI.style [("float","right"), ("height","2.2em")]
       tabs <- UI.div    # set UI.style [("background-color","#D3D3D3")]  #+ (map return [tab1,tab2,tab3,tab4,tab5,tab6,logo])

       let coloActive tab = do mapM_ (\t -> (return t) # set UI.style otherTab) [tab1,tab2,tab3,tab4,tab5,tab6]; return tab # set UI.style activeTab

       help <- guiHelp # set UI.style [("margin-top","0.5em")]
       on UI.click tab1 $ \_ -> do
            coloActive tab1
            UI.getBody window # set UI.children [tabs,help]
       on UI.click tab2 $ \_ -> do
            coloActive tab2
            pane <- guiObserve compTreeRef currentVertexRef # set UI.style [("margin-top","0.5em")]
            UI.getBody window # set UI.children [tabs,pane]
       on UI.click tab3 $ \_ -> do
            coloActive tab3
            pane <- guiAlgoDebug compTreeRef currentVertexRef regexRef imgCountRef # set UI.style [("margin-top","0.5em")]
            UI.getBody window # set UI.children [tabs,pane]
       on UI.click tab4 $ \_ -> do
            coloActive tab4
            pane <- guiAssisted traceRef propositions compTreeRef currentVertexRef regexRef imgCountRef # set UI.style [("margin-top","0.5em")]
            UI.getBody window # set UI.children [tabs,pane]
       on UI.click tab5 $ \_ -> do
            coloActive tab5
            pane <- guiExplore compTreeRef currentVertexRef regexRef imgCountRef # set UI.style [("margin-top","0.5em")]
            UI.getBody window # set UI.children [tabs,pane]
       on UI.click tab6 $ \_ -> do
            coloActive tab6
            pane <- guiStats compTreeRef
            UI.getBody window # set UI.children [tabs,pane]

       UI.getBody window # set UI.style [("margin","0")] #+ (map return [tabs,help])
       return ()


activeTab = ("background-color", "white") : tabstyle
otherTab  = ("background-color", "#f0f0f0") : tabstyle
tabstyle = [("-webkit-border-top-left-radius", "19"), ("-moz-border-top-left-radius", "19"), ("border-top-left-radius", "0.5em"),("-webkit-border-top-right-radius", "19"), ("-moz-border-top-right-radius", "19"), ("border-top-right-radius", "0.5em"), ("border-width", "medium medium 0px"),("margin-top","1em")]

--------------------------------------------------------------------------------
-- The help/welcome page

guiHelp :: UI UI.Element
guiHelp = UI.div # set UI.style [("margin-left", "20%"),("margin-right", "20%")] #+ 
  [ UI.h1 # set UI.text ("Welcome to Hoed " ++ showVersion version)
  , UI.p # set UI.text "Hoed is a tracer and debugger for the language Haskell. You can trace a program by annotating functions in suspected modules. After running the program the trace can be viewed in different ways using a web browser. Use the tabs at the top of this page to select the view you want to use. Below we give a short explenation of each view."
  , UI.h2 # set UI.text "Observe"
  , UI.p # set UI.text "The observe view is useful to get a first impression of what is happening in your program, or to get an overview of the computation statements of a particular slice or pattern. At the top the list of slices and for each slice how many times it was reduced. Below the line a list of computation statements."
  , UI.h2 # set UI.text "Algorithmic Debugging"
  , UI.p # set UI.text "The algorithmic debugger shows you recorded computation statements, that is a function applied to an argument and its result. You judge these statements as right or wrong. When enough statements are judged the debugger tells you the location of the fault in your code."
  , UI.h2 # set UI.text "Explore"
  , UI.p # set UI.text "The trace is translated into a tree of computation statements for the algorithmic debugging view. In the explore view you can freely browse this tree to get a better understanding of your program. You can decide yourself in which order you want to judge statements. When enough statements are judged the debugger tells you the location of the fault in your code."
  ] 

--------------------------------------------------------------------------------
-- The statistics GUI

guiStats :: IORef CompTree -> UI UI.Element
guiStats compTreeRef = do
  (Graph _ vs as) <- UI.liftIO $ readIORef compTreeRef

  sec1   <- UI.h3 # set UI.text "Computation Tree Statistics"
  -- Minus 1 for the root node
  vcount <- UI.div # set UI.text (show ((length vs) - 1) ++ " computation statements")
  -- TODO: Should we subtract the dependencies from the root node?
  acount <- UI.div # set UI.text (show (length as) ++ " dependencies")

  sec2   <- UI.h3 # set UI.text "Judgements"
  -- Minus 1 for the root node that is always considered as right
  rightCount <- UI.div # set UI.text ((show $ (length . filter isRight $ vs) - 1) ++ " Right")
  wrongCount <- UI.div # set UI.text ((show . length . filter isWrong      $ vs) ++ " Wrong")
  assCount   <- UI.div # set UI.text ((show . length . filter isAssisted   $ vs) ++ " Assisted")
  incon      <- UI.div # set UI.text ((show . length . filter isInconclusive $ vs) ++ " Inconclusive") # set UI.style [("margin-left","2em")]

  passing    <- UI.div # set UI.text ((show . length . filter isPassing $ vs) ++ " Passing") # set UI.style [("margin-left","2em")]
  unassCount <- UI.div # set UI.text ((show . length . filter isUnassessed $ vs) ++ " Unassessed")

  UI.div # set UI.style [("margin-left", "20%"),("margin-right", "20%")] #+ (map return [sec1,vcount,acount, sec2, rightCount, wrongCount, assCount, incon, passing, unassCount])

--------------------------------------------------------------------------------
-- The observe GUI

guiObserve :: IORef CompTree -> IORef Int -> UI UI.Element
guiObserve compTreeRef currentVertexRef = do
       (Graph _ vs _) <- UI.liftIO $ readIORef compTreeRef

       -- Alphabetical sorted list of slices, and for each slice how many computation statements
       -- there are for that slice
       let slices' = sort $ map (stmtLabel . vertexStmt) . filter (not . isRootVertex) $ vs
           slices  = nub slices'
           count slice = length (filter (==slice) slices')
           span s = UI.span # set UI.text s # set UI.style [("margin-right","1em")]
           spans = map (\(c,lbl) -> span $ show c ++ " " ++ lbl) $ zip (map count slices) slices

       -- Alphabetical sorted list of computation statements
       let vs_sorted = sortOn (vertexRes) . filter (not . isRootVertex) $ vs
       stmtDiv <- UI.form # set UI.style [("margin-left","2em")]
       updateRegEx currentVertexRef vs_sorted stmtDiv "" -- with empty regex to fill div3 1st time

       -- The regexp filter
       regexRef <- UI.liftIO $ newIORef ""
       matchField  <- UI.input
       matchButton <- UI.button # UI.set UI.text "search"
       -- Uncomment next line to search automatically when the user changes the regex
       -- on UI.valueChange matchField (updateRegEx currentVertexRef vs_sorted stmtDiv)
       on UI.valueChange matchField $ \s -> UI.liftIO $ writeIORef regexRef s
       on UI.click matchButton $ \_ -> do
            r <- UI.liftIO $ readIORef regexRef
            updateRegEx currentVertexRef vs_sorted stmtDiv r

       UI.div  #+ (spans ++ [UI.hr, UI.span # set UI.text "regex filter: ", return matchField, return matchButton, UI.hr, return stmtDiv])

updateRegEx :: IORef Int -> [Vertex] -> UI.Element -> String -> UI ()
updateRegEx currentVertexRef vs stmtDiv r = do
  (return stmtDiv) # set UI.text "Applying filter ..."
  rComp <- UI.liftIO $ compile defaultCompOpt defaultExecOpt r
  case rComp of Prelude.Right _                 -> drawR
                Prelude.Left  (_, errorMessage) -> drawL errorMessage
  return ()

  where 
  drawL m = do (return stmtDiv) # set UI.text m
  drawR
    | vs_filtered == []  = drawL $ "There are no computation statements matching \"" ++ r ++ "\"."
    | otherwise          = (return stmtDiv) # set UI.children [] #+ csDivs

  vs_filtered = if r == "" then vs else filter (\v -> (noNewlines . vertexRes $ v) =~ r) vs

  csDivs = map stmtToDiv vs_filtered

  stmtToDiv v = do
    i <- UI.liftIO $ readIORef currentVertexRef
    s <- UI.span # set UI.text (vertexRes v)
    r <- UI.input # set UI.type_ "radio" # set UI.checked (i == vertexUID v)
    let src | isRight v = "static/right.png"
            | isWrong v = "static/wrong.png"
            | otherwise = "static/unassessed.png"
    j <- UI.img # set UI.src src # set UI.height 20 # set UI.style [("margin-left","1em")]
    on UI.checkedChange r $ \_ -> checked v
    UI.div #+ map return [r,s,j]

  checked v = do
    UI.liftIO $ writeIORef currentVertexRef (vertexUID v)
    drawR

--------------------------------------------------------------------------------
-- The Assisted Debugging GUI

guiAssisted :: IORef Trace -> [Propositions] -> IORef CompTree -> IORef Int -> IORef String -> IORef Int -> UI UI.Element
guiAssisted traceRef ps compTreeRef currentVertexRef regexRef imgCountRef = do

       handlerRef <- UI.liftIO $ newIORef Bottom

       -- Get a list of vertices from the computation tree
       tree <- UI.liftIO $ readIORef compTreeRef

       -- Status
       status <- UI.span
       updateStatus status compTreeRef 

       -- Field to show current computation statement to the programmer
       compStmt <- UI.pre # set UI.style [("margin","0 2em")]
       showStmt compStmt compTreeRef currentVertexRef 

       -- Buttons to judge the current statement
       right <- UI.button # UI.set UI.text "right " #+ [UI.img # set UI.src "static/right.png" # set UI.height 20] # set UI.style [("margin-right","1em")]
       wrong <- UI.button # set UI.text "wrong "    #+ [UI.img # set UI.src "static/wrong.png" # set UI.height 20] # set UI.style [("margin-right","1em")]
       let j = judge AdvanceToNext status compStmt Nothing Nothing currentVertexRef compTreeRef
       j right Right
       j wrong Wrong
       let lightBulbs n = take n . repeat  $ UI.img # set UI.src "static/test.png" # set UI.height 20

       test1B   <- UI.button # set UI.text "test" #+ lightBulbs 1 # set UI.style [("margin-right","1em")]
       testChB  <- UI.button # set UI.text "test" #+ lightBulbs 2 # set UI.style [("margin-right","1em")]
       testAllB <- UI.button # set UI.text "test" #+ lightBulbs 3 # set UI.style [("margin-right","1em")]
       resetB <- UI.button # set UI.text "clear all" # set UI.height 20
       on UI.click test1B   $ \_ -> testCurrent compTreeRef traceRef ps status currentVertexRef compStmt handlerRef
       on UI.click testChB  $ \_ -> testCurrentAndChildren compTreeRef traceRef ps status currentVertexRef compStmt handlerRef
       on UI.click testAllB $ \_ -> testAll compTreeRef traceRef ps status currentVertexRef compStmt handlerRef
       on UI.click resetB   $ \_ -> resetTree compTreeRef ps status currentVertexRef compStmt

       -- Populate the main screen
       top <- UI.center #+ [return status, UI.br, return right, return wrong, return test1B, return testChB, return testAllB, return resetB]
       UI.div #+ [return top, UI.hr, return compStmt]

resetTree compTreeRef ps status currentVertexRef compStmt = do
  t <- UI.liftIO $ readIORef compTreeRef
  UI.liftIO $ writeIORef compTreeRef (mapGraph resetVertex t)
  advance AdvanceToNext status compStmt Nothing Nothing currentVertexRef compTreeRef
  where resetVertex = flip setJudgement Unassessed

testCurrent :: IORef CompTree -> IORef Trace -> [Propositions] -> UI.Element -> IORef Int -> UI.Element -> t -> UI ()
testCurrent compTreeRef traceRef ps status currentVertexRef compStmt handlerRef = do
  return status # UI.set UI.text "Evaluating propositions ..." #+ [UI.img # set UI.src "static/loading.gif" # set UI.height 30]
  mcv <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
  case mcv of
    Nothing -> updateStatus status compTreeRef
    (Just cv) -> do
      time1 <- UI.liftIO getCurrentTime
      test1 cv compTreeRef traceRef ps handlerRef (onNoProps cv) onJudge onTreeSwitch
      time2 <- UI.liftIO getCurrentTime
      UI.liftIO $ putStrLn ("tested computation statement in " ++ show (diffUTCTime time2 time1))
  where
  onNoProps cv = do
    UI.element status # set UI.text ("Cannot test: no propositions associated with function " ++ (stmtLabel . vertexStmt) cv ++ "!")
    return ()
  onJudge =
    advance AdvanceToNext status compStmt Nothing Nothing currentVertexRef compTreeRef
  onTreeSwitch :: CompTree -> Trace -> UI ()
  onTreeSwitch newCompTree newTrace = do
      UI.liftIO $ writeIORef compTreeRef newCompTree
      UI.liftIO $ writeIORef traceRef newTrace 
      advance AdvanceToNext status compStmt Nothing Nothing currentVertexRef compTreeRef
      return status # UI.set UI.text "Discovered and switched to a simpler computatation tree!" #+ [UI.img # set UI.src "static/test.png" # set UI.height 30]
      return ()

testCurrentAndChildren compTreeRef traceRef ps status currentVertexRef compStmt handlerRef = do
  return status # UI.set UI.text "Evaluating propositions ..." #+ [UI.img # set UI.src "static/loading.gif" # set UI.height 30]
  mcv <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
  compTree <- UI.liftIO $ readIORef compTreeRef
  case mcv of
    Nothing -> updateStatus status compTreeRef
    (Just cv) -> do
      let vs = cv : succs compTree cv
      time1 <- UI.liftIO getCurrentTime
      switchedTree <- testMany vs compTreeRef traceRef ps handlerRef
      time2 <- UI.liftIO getCurrentTime
      UI.liftIO $ putStrLn ("tested " ++ show (length vs) ++ " computation statements in " ++ show (diffUTCTime time2 time1))
      advance AdvanceToNext status compStmt Nothing Nothing currentVertexRef compTreeRef
      if switchedTree 
      then do
        return status # UI.set UI.text "Discovered and switched to a simpler computatation tree!" #+ [UI.img # set UI.src "static/test.png" # set UI.height 30]
        return ()
      else
        return ()

testAll compTreeRef traceRef ps status currentVertexRef compStmt handlerRef = do
  return status # UI.set UI.text "Evaluating propositions ..." #+ [UI.img # set UI.src "static/loading.gif" # set UI.height 30]
  mcv <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
  compTree <- UI.liftIO $ readIORef compTreeRef
  let vs = vertices compTree
  time1 <- UI.liftIO getCurrentTime
  switchedTree <- testMany vs compTreeRef traceRef ps handlerRef
  time2 <- UI.liftIO getCurrentTime
  UI.liftIO $ putStrLn ("tested " ++ show (length vs) ++ " computation statements in " ++ show (diffUTCTime time2 time1))
  advance AdvanceToNext status compStmt Nothing Nothing currentVertexRef compTreeRef
  if switchedTree 
  then do
    return status # UI.set UI.text "Discovered and switched to a simpler computatation tree!" #+ [UI.img # set UI.src "static/test.png" # set UI.height 30]
    return ()
  else
    return ()

testMany :: [Vertex] -> IORef CompTree -> IORef Trace -> [Propositions] -> t -> UI Bool
testMany vs compTreeRef traceRef ps handlerRef = case vs of
  []      -> return False
  (v:vs') -> do
    continue <- test1 v compTreeRef traceRef ps handlerRef (onNoProps v) onJudge onTreeSwitch
    if continue then testMany vs' compTreeRef traceRef ps handlerRef
                else return True
  where
  onNoProps _ =
    return True
  onJudge = 
    return True
  onTreeSwitch newCompTree newTrace = do
    UI.liftIO $ writeIORef compTreeRef newCompTree
    UI.liftIO $ writeIORef traceRef newTrace 
    UI.liftIO $ putStrLn $ "Switched to a simpler computation tree!"
    return False

test1 :: Vertex -> IORef CompTree -> IORef Trace -> [Propositions] -> t -> UI b -> UI b -> (CompTree -> Trace -> UI b) -> UI b
test1 vertex compTreeRef traceRef ps handlerRef onNoProps onJudge onTreeSwitch = do
  compTree <- UI.liftIO $ readIORef compTreeRef
  mj <- test1' vertex compTree traceRef ps handlerRef
  case mj of
    Nothing  -> onNoProps
    (Just j) -> case j of 
      (Judge jmt) -> do
        UI.liftIO $ writeIORef compTreeRef (replaceVertex compTree (setJudgement vertex jmt))
        onJudge
      (AlternativeTree compTree' trace') -> do
        onTreeSwitch compTree' trace'

test1' :: Vertex -> CompTree -> IORef Trace -> [Propositions] -> t -> UI (Maybe Judge)
test1' v compTree traceRef ps handlerRef = do
      case lookupPropositions ps v of 
        Nothing  ->
          return Nothing
        (Just p) -> do
          trace <- UI.liftIO $ readIORef traceRef
          j <- UI.liftIO $ Prop.judge trace p v unjudgedCharacterCount compTree
          return (Just j)


--------------------------------------------------------------------------------
-- The Algorithmic Debugging GUI

guiAlgoDebug :: IORef CompTree -> IORef Int -> IORef String -> IORef Int -> UI UI.Element
guiAlgoDebug compTreeRef currentVertexRef regexRef imgCountRef = do

       -- Get a list of vertices from the computation tree
       tree <- UI.liftIO $ readIORef compTreeRef

       -- Status
       status <- UI.span
       updateStatus status compTreeRef 

       -- Field to show computation statement(s) of current vertex
       compStmt <- UI.pre # set UI.style [("margin","0 2em")]
       showStmt compStmt compTreeRef currentVertexRef 

       -- Buttons to judge the current statement
       right <- UI.button # UI.set UI.text "right " #+ [UI.img # set UI.src "static/right.png" # set UI.height 30] # set UI.style [("margin-right","1em")]
       wrong <- UI.button # set UI.text "wrong "    #+ [UI.img # set UI.src "static/wrong.png" # set UI.height 30]
       let j = judge AdvanceToNext status compStmt Nothing Nothing currentVertexRef compTreeRef
       j right Right
       j wrong Wrong

       -- Populate the main screen
       top <- UI.center #+ [return status, UI.br, return right, return wrong]
       UI.div #+ [return top, UI.hr, return compStmt]

--------------------------------------------------------------------------------
-- Judge a computation statement, shared between the algorithmic debugging
-- view and explore view.

data Advance = AdvanceToNext | DoNotAdvance

judge :: Advance -> UI.Element -> UI.Element -> Maybe UI.Element -> Maybe (UI.Element,IORef Int) 
         -> IORef UID -> IORef CompTree -> UI.Element -> Judgement  -> UI ()
judge adv status compStmt mMenu mImg currentVertexRef compTreeRef b j = 
  on UI.click b $ \_ -> do
    mv <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
    case mv of
      (Just v) -> judge' v
      Nothing  -> return ()
  where 
  judge' :: Vertex -> UI ()
  judge' v = do
      t' <- UI.liftIO $ readIORef compTreeRef
      UI.liftIO $ writeIORef compTreeRef (markNode t' v j)
      advance adv status compStmt mMenu mImg currentVertexRef compTreeRef

advance :: Advance -> UI.Element -> UI.Element -> Maybe UI.Element -> Maybe (UI.Element,IORef Int) 
         -> IORef UID -> IORef CompTree -> UI ()
advance adv status compStmt mMenu mImg currentVertexRef compTreeRef = do
  mv <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
  case mv of
    Nothing  -> do
      t <- UI.liftIO $ readIORef compTreeRef
      case next_step t getJudgement of
        RootVertex -> return () -- when does this happen ... ?
        w          -> advanceTo w status compStmt mMenu mImg currentVertexRef compTreeRef
    (Just v) -> do
      t <- UI.liftIO $ readIORef compTreeRef
      case (adv, next_step t getJudgement) of
        (DoNotAdvance,_)           -> advanceTo v status compStmt mMenu mImg currentVertexRef compTreeRef
        (AdvanceToNext,RootVertex) -> advanceTo v status compStmt mMenu mImg currentVertexRef compTreeRef
        (AdvanceToNext,w)          -> advanceTo w status compStmt mMenu mImg currentVertexRef compTreeRef
  updateStatus status compTreeRef 

advanceTo w status compStmt mMenu mImg currentVertexRef compTreeRef = do
  UI.liftIO $ writeIORef currentVertexRef (vertexUID w)
  showStmt compStmt compTreeRef currentVertexRef
  case mMenu of Nothing                  -> return ()
                (Just menu)              -> updateMenu menu compTreeRef currentVertexRef 
  case mImg  of Nothing                  -> return ()
                (Just (img,imgCountRef)) -> redraw img imgCountRef compTreeRef (Just w)


--------------------------------------------------------------------------------
-- Explore the computation tree

guiExplore :: IORef CompTree -> IORef Int -> IORef String -> IORef Int -> UI UI.Element
guiExplore compTreeRef currentVertexRef regexRef imgCountRef = do

       -- Get a list of vertices from the computation graph
       tree <- UI.liftIO $ readIORef compTreeRef

       -- Draw the computation graph
       img  <- UI.img 
       redrawWith img imgCountRef compTreeRef currentVertexRef
       img' <- UI.center #+ [UI.element img]

       -- Field to show computation statement(s) of current vertex
       compStmt <- UI.pre

       -- Menu to select which statement to show
       menu <- UI.select # set UI.style [("margin-right","1em")]
       showStmt compStmt compTreeRef currentVertexRef
       updateMenu menu compTreeRef currentVertexRef 
       let selectVertex' = selectVertex compStmt menu compTreeRef currentVertexRef (redrawWith img imgCountRef compTreeRef)
       on UI.selectionChange menu selectVertex'

       -- Status
       status <- UI.span
       updateStatus status compTreeRef 

       -- Buttons to judge the current statement
       right <- UI.button # UI.set UI.text "right " #+ [UI.img # set UI.src "static/right.png" # set UI.height 20] # set UI.style [("margin-right","1em")]
       wrong <- UI.button # set UI.text "wrong "    #+ [UI.img # set UI.src "static/wrong.png" # set UI.height 20] # set UI.style [("margin-right","1em")]

       let j = judge DoNotAdvance status compStmt (Just menu) (Just (img, imgCountRef)) currentVertexRef compTreeRef
       j right Right
       j wrong Wrong

       -- Store and restore buttons
       storeb   <- UI.button # UI.set UI.text "store" # set UI.style [("margin-right","1em")]
       restoreb <- UI.button # UI.set UI.text "restore" 
       on UI.click storeb $ \_ -> UI.liftIO $ store compTreeRef
       on UI.click restoreb $ \_ -> do UI.liftIO $ restore compTreeRef
                                       updateStatus status compTreeRef
                                       v <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
                                       redraw img imgCountRef compTreeRef v

       -- Populate the main screen
       hr <- UI.hr
       br <- UI.br
       UI.div #+ (map UI.element [menu, right, wrong, storeb, restoreb, br, hr, img', br, status, hr, compStmt])

judgementFilePath :: FilePath
judgementFilePath = ".Hoed/savedJudgements"

store :: IORef CompTree -> IO ()
store compTreeRef = do
  t <- readIORef compTreeRef
  storeJudgements judgementFilePath t

restore :: IORef CompTree -> IO ()
restore compTreeRef = do
  t  <- readIORef compTreeRef
  t' <- restoreJudgements judgementFilePath t
  writeIORef compTreeRef t'

preorder :: CompTree -> [Vertex]
preorder = getPreorder . getDfs

showStmt :: UI.Element -> IORef CompTree -> IORef Int -> UI ()
showStmt e compTreeRef currentVertexRef = do 
  mv <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
  let s = case mv of
                Nothing  -> "No computation statement selected."
                (Just v) -> let s = show . vertexStmt $ v in case getJudgement v of
                              (Assisted s') -> "Results from testing computation statement:\n" ++ (getMessage s') ++ "\n---\n\n" ++ s
                              _             -> s
  UI.element e # set UI.text s
  return ()

getMessage :: [AssistedMessage] -> String
getMessage = foldl (\acc m -> acc ++ getMessage' m) ""
getMessage' (InconclusiveProperty msg) = msg
getMessage' (PassingProperty msg) = msg ++ " holds for this statement's values"

-- populate the exploration menu with the current vertex, its predecessor and its successors
updateMenu :: UI.Element -> IORef CompTree -> IORef Int -> UI ()
updateMenu menu compTreeRef currentVertexRef = do
       vs <- menuVertices compTreeRef currentVertexRef
       i  <- UI.liftIO $ readIORef currentVertexRef
       let j = case findIndex (\v -> vertexUID v == i) vs of
                 (Just j') -> j'
                 Nothing   -> 0
       t  <- UI.liftIO $ readIORef compTreeRef
       let fs = faultyVertices t
       ops <- mapM (\s->UI.option # set UI.text s) $ map (summarizeVertex fs) vs
       (UI.element menu) # set UI.children []
       UI.element menu   #+ (map UI.element ops)
       (UI.element menu) # set UI.selection (Just j)
       return ()

-- on selecting a vertex in the exploration menu, update current vertex accordingly
selectVertex :: UI.Element -> UI.Element -> IORef CompTree -> IORef Int  
        -> (IORef Int -> UI ()) -> Maybe Int -> UI ()
selectVertex compStmt menu compTreeRef currentVertexRef myRedraw mi = case mi of
        Just j  -> do vs    <- menuVertices compTreeRef currentVertexRef
                      mcv   <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
                      let v  = vs !! j
                      UI.liftIO $ writeIORef currentVertexRef (vertexUID v)
                      showStmt compStmt compTreeRef currentVertexRef
                      myRedraw currentVertexRef 
                      updateMenu menu compTreeRef currentVertexRef
                      return ()
        Nothing -> do UI.liftIO $ putStrLn "selectVertex: Nothing selected"
                      return ()

menuVertices :: IORef CompTree -> IORef Int -> UI [Vertex]
menuVertices compTreeRef currentVertexRef = do
  t   <- UI.liftIO $ readIORef compTreeRef
  i   <- UI.liftIO $ readIORef currentVertexRef
  mcv <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
  let cv = case mcv of (Just v) -> v; Nothing -> RootVertex
      ps = preds t cv
      sibl = if RootVertex `elem` ps then succs t RootVertex else [cv]
  return $ filter (/= RootVertex) $ ps ++ sibl ++ (succs t cv)

lookupCurrentVertex :: IORef Int -> IORef CompTree -> IO (Maybe Vertex)
lookupCurrentVertex currentVertexRef compTree = do
  i <- readIORef currentVertexRef
  t <- readIORef compTree
  return $ case filter (\v->vertexUID v==i) (vertices t) of
                 []  -> Nothing
                 [v] -> Just v
                 vs   -> error $ "lookupCurrentVertex: UID " ++ show i ++ " identifies "
                                 ++ (show . length $ vs) ++ " computation statements"

markNode :: CompTree -> Vertex -> Judgement -> CompTree
markNode g v s = mapGraph f g
  where f RootVertex = RootVertex
        f v'         = if v' === v then setJudgement v s else v'

        (===) :: Vertex -> Vertex -> Bool
        v1 === v2 = (vertexUID v1) == (vertexUID v2)

data MaxStringLength = ShorterThan Int | Unlimited

shorten :: MaxStringLength -> String -> String
shorten Unlimited s = s
shorten (ShorterThan l) s
          | length s < l = s
          | l > 3        = take (l - 3) s ++ "..."
          | otherwise    = take l s

shorterThan60 = shorten (ShorterThan 60)

summarizeVertex :: [Vertex] -> Vertex -> String
summarizeVertex fs v = shorterThan60 (noNewlines . show . vertexStmt $ v) ++ s
  where s = if v `elem` fs then " !!" else case getJudgement v of
              Wrong          -> " :("
              Right          -> " :)"
              _              -> " ??"

vertexGraphvizLabel :: [Vertex] -> Vertex -> String
vertexGraphvizLabel fs v =
  "<<TABLE BORDER=\"0\" CELLBORDER=\"0\"><TR><TD HEIGHT=\"30\" WIDTH=\"30\" FIXEDSIZE=\"true\"><IMG SCALE=\"true\" SRC=\"" ++ (vertexImg fs v) ++ "\"/></TD><TD><FONT POINT-SIZE=\"30\">" ++ (htmlEscape . shorterThan60 . noNewlines . show . vertexStmt $ v) ++ "</FONT></TD></TR></TABLE>>"

htmlEscape :: String -> String
htmlEscape = foldr (\c acc -> replace c ++ acc) ""
  where
  replace :: Char -> String
  replace '"'  = "&quot;"
  replace '{'  = "&#123;"
  replace '\\' = "&#92;"
  replace '>'  = "&gt;"
  replace '<'  = "&lt;"
  replace '}'  = "&#125;"
  replace c    = [c]

vertexImg :: [Vertex] -> Vertex -> String
vertexImg fs v = if v `elem` fs then ".Hoed/wwwroot/faulty.png" else case vertexJmt v of
              Unassessed   -> ".Hoed/wwwroot/unassessed.png"
              (Assisted _) -> ".Hoed/wwwroot/unassessed.png"
              Wrong        -> ".Hoed/wwwroot/wrong.png"
              Right        -> ".Hoed/wwwroot/right.png"


updateStatus :: UI.Element -> IORef CompTree -> UI ()
updateStatus e compGraphRef = do
  g <- UI.liftIO $ readIORef compGraphRef
  let isJudged v = case getJudgement v of
        Right -> True
        Wrong -> True
        _     -> False -- Assisted does not count as judged
      slen       = show . length
      ns = filter (not . isRootVertex) (preorder g)
      js = filter isJudged ns
      fs = faultyVertices g
      txt = if length fs > 0 then " Fault detected in: " ++ (vertexRes . head) fs
                             else " Judged " ++ slen js ++ "/" ++ slen ns
  UI.element e # set UI.text txt
  return ()


redrawWith :: UI.Element -> IORef Int -> IORef CompTree -> IORef Int -> UI ()
redrawWith img imgCountRef compTreeRef currentVertexRef = do
  mv <- UI.liftIO $ lookupCurrentVertex currentVertexRef compTreeRef
  redraw img imgCountRef compTreeRef mv

redraw :: UI.Element -> IORef Int -> IORef CompTree -> (Maybe Vertex) -> UI ()
redraw img imgCountRef compTreeRef mcv
  = do tree <- UI.liftIO $ readIORef compTreeRef
       UI.liftIO $ writeFile ".Hoed/debugTree.dot" $ shw (faultyVertices tree) (summarize tree mcv)
       UI.liftIO $ system $ "cat .Hoed/debugTree.dot | unflatten -l 5| dot -Tpng -Gsize=15,15 -Gdpi=100"
                            ++ "> .Hoed/wwwroot/debugTree.png"
       i <- UI.liftIO $ readIORef imgCountRef
       UI.liftIO $ writeIORef imgCountRef (i+1)
       -- Attach counter to image url to reload image
       UI.element img # set UI.src ("static/debugTree.png#" ++ show i)
       return ()

  where shw fs t = showWith t (coloVertex $ fs) showArc
        coloVertex _ RootVertex = ("\".\"", "shape=none")
        coloVertex fs v = ( vertexGraphvizLabel fs v
                          , if isCurrentVertex mcv v then "shape=none fontcolor=blue"
                                                     else "shape=none"
                          )
        showArc _  = ""

-- Selects current vertex, its predecessor and its successors
summarize :: CompTree -> Maybe Vertex -> CompTree
summarize tree (Just cv) = Graph r vs as'
  where 
  i    = vertexUID cv
  ps   = preds tree cv
  ps'  = if RootVertex `elem` ps then ps ++ (succs tree RootVertex) else ps
  cs   = succs tree cv
  vs   = nub (ps' ++ cv : cs)
  as   = filter (\a -> isCV (source a) || isCV (target a)) (arcs tree)
  as'  = if RootVertex `elem` ps then nub (as ++ filter (\a -> isRV (source a)  || isRV (target a)) (arcs tree)) else as
  r    = if RootVertex `elem` vs then RootVertex else head vs
  isCV = (==i) . vertexUID
  isRV = (==) RootVertex
summarize tree Nothing   = tree

isCurrentVertex :: Maybe Vertex -> Vertex -> Bool
isCurrentVertex mcv v = case v of
  RootVertex -> False
  _    -> case mcv of 
                Nothing           -> False
                (Just RootVertex) -> False
                (Just w)          -> vertexStmt v == vertexStmt w

faultyVertices :: CompTree -> [Vertex]
faultyVertices = findFaulty_dag getJudgement
