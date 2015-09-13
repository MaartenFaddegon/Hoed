import Properties
import Debug.Hoed.Pure
import System.Environment
import Text.Printf
import Control.Monad
import System.Random
import Test.QuickCheck
import Data.Maybe
import XMonad.StackSet
import qualified Data.Map as M

main :: IO ()
main = logOwp "hoed-tests-Prop-t3.graph" propositions $ do
{-
    args <- fmap (drop 1) getArgs
    let n = if null args then 100 else read (head args)
    (results, passed) <- liftM unzip $ mapM (\(s,a) -> printf "%-40s: " s >> a n) tests
    printf "Passed %d tests!\n" (sum passed)
    when (not . and $ results) $ fail "Not all tests passed!"
-}
 
    g <- newStdGen; print . fromJust . ok . (generate 1 g) . evaluate $  prop_greedyView_reversible (NonNegative 1) (StackSet {current = Screen {workspace = Workspace {tag = NonNegative 4, layout = 2, stack = Nothing}, screen = 0, screenDetail = 2}, visible = [Screen {workspace = Workspace {tag = NonNegative 1, layout = 2, stack = Just (Stack {focus = 'f', up = "", down = "oda"})}, screen = 1, screenDetail = 0},Screen {workspace = Workspace {tag = NonNegative 2, layout = 2, stack = Just (Stack {focus = 'w', up = "", down = "n"})}, screen = 2, screenDetail = 1},Screen {workspace = Workspace {tag = NonNegative 3, layout = 2, stack = Just (Stack {focus = 't', up = "", down = "e"})}, screen = 3, screenDetail = -1}], hidden = [Workspace {tag = NonNegative 4, layout = 2, stack = Nothing},Workspace {tag = NonNegative 5, layout = 2, stack = Nothing},Workspace {tag = NonNegative 6, layout = 2, stack = Nothing},Workspace {tag = NonNegative 7, layout = 2, stack = Nothing},Workspace {tag = NonNegative 8, layout = 2, stack = Nothing}], floating = M.fromList []})


 where
    propositions =
        [ Propositions [ (QuickCheckProposition,module_Properties,"prop_view_reversible",[1,0])
                       -- Note how the arguments are swapped for prop_view_current
                       -- , (QuickCheckProposition,module_Properties,"prop_view_current",[0,1])
                       ] 
                       PropertiesOf "view" [module_StackSet, module_QuickCheck, module_Map, module_Random, module_Maybe]
        , Propositions [ (QuickCheckProposition,module_Properties,"prop_greedyView_reversible",[1,0])
                       ] 
                       PropertiesOf "greedyView" [module_StackSet, module_QuickCheck, module_Map, module_Random, module_Maybe]

        ]

    module_Properties = Module "Properties"              "../Prop/t3/"
    module_StackSet   = Module "XMonad.StackSet"         "../Prop/t3/"
    module_QuickCheck = Module "Test.QuickCheck"         "../Prop/t3/"
    module_Map        = Module "qualified Data.Map as M" ""
    module_Random     = Module "System.Random"           ""
    module_Maybe      = Module "Data.Maybe"              ""

    tests =
        [ -- ("view reversible"    , mytest prop_view_reversible),
          ("greedyView reversible"     , mytest prop_greedyView_reversible)
        ]

{-
        [("StackSet invariants" , mytest prop_invariant)

        ,("empty: invariant"    , mytest prop_empty_I)
        ,("empty is empty"      , mytest prop_empty)
        ,("empty / current"     , mytest prop_empty_current)
        ,("empty / member"      , mytest prop_member_empty)

        ,("view : invariant"    , mytest prop_view_I)
        ,("view sets current"   , mytest prop_view_current)
        ,("view idempotent"     , mytest prop_view_idem)
        ,("view reversible"    , mytest prop_view_reversible)
--      ,("view / xinerama"     , mytest prop_view_xinerama)
        ,("view is local"       , mytest prop_view_local)

        ,("greedyView : invariant"    , mytest prop_greedyView_I)
        ,("greedyView sets current"   , mytest prop_greedyView_current)
        ,("greedyView is safe "   ,   mytest prop_greedyView_current_id)
        ,("greedyView idempotent"     , mytest prop_greedyView_idem)
        ,("greedyView reversible"     , mytest prop_greedyView_reversible)
        ,("greedyView is local"       , mytest prop_greedyView_local)
--
--      ,("valid workspace xinerama", mytest prop_lookupWorkspace)

        ,("peek/member "        , mytest prop_member_peek)

        ,("index/length"        , mytest prop_index_length)

        ,("focus left : invariant", mytest prop_focusUp_I)
        ,("focus master : invariant", mytest prop_focusMaster_I)
        ,("focus right: invariant", mytest prop_focusDown_I)
        ,("focusWindow: invariant", mytest prop_focus_I)
        ,("focus left/master"   , mytest prop_focus_left_master)
        ,("focus right/master"  , mytest prop_focus_right_master)
        ,("focus master/master"  , mytest prop_focus_master_master)
        ,("focusWindow master"  , mytest prop_focusWindow_master)
        ,("focus left/right"    , mytest prop_focus_left)
        ,("focus right/left"    , mytest prop_focus_right)
        ,("focus all left  "    , mytest prop_focus_all_l)
        ,("focus all right "    , mytest prop_focus_all_r)
        ,("focus down is local"      , mytest prop_focus_down_local)
        ,("focus up is local"      , mytest prop_focus_up_local)
        ,("focus master is local"      , mytest prop_focus_master_local)
        ,("focus master idemp"  , mytest prop_focusMaster_idem)

        ,("focusWindow is local", mytest prop_focusWindow_local)
        ,("focusWindow works"   , mytest prop_focusWindow_works)
        ,("focusWindow identity", mytest prop_focusWindow_identity)

        ,("findTag"           , mytest prop_findIndex)
        ,("allWindows/member"   , mytest prop_allWindowsMember)
        ,("currentTag"          , mytest prop_currentTag)

        ,("insert: invariant"   , mytest prop_insertUp_I)
        ,("insert/new"          , mytest prop_insert_empty)
        ,("insert is idempotent", mytest prop_insert_idem)
        ,("insert is reversible", mytest prop_insert_delete)
        ,("insert is local"     , mytest prop_insert_local)
        ,("insert duplicates"   , mytest prop_insert_duplicate)
        ,("insert/peek "        , mytest prop_insert_peek)
        ,("insert/size"         , mytest prop_size_insert)

        ,("delete: invariant"   , mytest prop_delete_I)
        ,("delete/empty"        , mytest prop_empty)
        ,("delete/member"       , mytest prop_delete)
        ,("delete is reversible", mytest prop_delete_insert)
        ,("delete is local"     , mytest prop_delete_local)
        ,("delete/focus"        , mytest prop_delete_focus)
        ,("delete  last/focus up", mytest prop_delete_focus_end)
        ,("delete ~last/focus down", mytest prop_delete_focus_not_end)

        ,("filter preserves order", mytest prop_filter_order)

        ,("swapMaster: invariant", mytest prop_swap_master_I)
        ,("swapUp: invariant" , mytest prop_swap_left_I)
        ,("swapDown: invariant", mytest prop_swap_right_I)
        ,("swapMaster id on focus", mytest prop_swap_master_focus)
        ,("swapUp id on focus", mytest prop_swap_left_focus)
        ,("swapDown id on focus", mytest prop_swap_right_focus)
        ,("swapMaster is idempotent", mytest prop_swap_master_idempotent)
        ,("swap all left  "     , mytest prop_swap_all_l)
        ,("swap all right "     , mytest prop_swap_all_r)
        ,("swapMaster is local" , mytest prop_swap_master_local)
        ,("swapUp is local"   , mytest prop_swap_left_local)
        ,("swapDown is local"  , mytest prop_swap_right_local)

        ,("shiftMaster id on focus", mytest prop_shift_master_focus)
        ,("shiftMaster is local", mytest prop_shift_master_local)
        ,("shiftMaster is idempotent", mytest prop_shift_master_idempotent)
        ,("shiftMaster preserves ordering", mytest prop_shift_master_ordering)

        ,("shift: invariant"    , mytest prop_shift_I)
        ,("shift is reversible" , mytest prop_shift_reversible)
        ,("shiftWin: invariant" , mytest prop_shift_win_I)
        ,("shiftWin is shift on focus" , mytest prop_shift_win_focus)
        ,("shiftWin fix current" , mytest prop_shift_win_fix_current)

        ,("floating is reversible" , mytest prop_float_reversible)
        ,("floating sets geometry" , mytest prop_float_geometry)
        ,("floats can be deleted", mytest prop_float_delete)
        ,("screens includes current", mytest prop_screens)

        ,("differentiate works", mytest prop_differentiate)
        ,("lookupTagOnScreen", mytest prop_lookup_current)
        ,("lookupTagOnVisbleScreen", mytest prop_lookup_visible)
        ,("screens works",      mytest prop_screens_works)
        ,("renaming works",     mytest prop_rename1)
        ,("ensure works",     mytest prop_ensure)
        ,("ensure hidden semantics",     mytest prop_ensure_append)

        ,("mapWorkspace id", mytest prop_mapWorkspaceId)
        ,("mapWorkspace inverse", mytest prop_mapWorkspaceInverse)
        ,("mapLayout id", mytest prop_mapLayoutId)
        ,("mapLayout inverse", mytest prop_mapLayoutInverse)

        -- testing for failure:
        ,("abort fails",            mytest prop_abort)
        ,("new fails with abort",   mytest prop_new_abort)
        ,("shiftWin identity",      mytest prop_shift_win_indentity)

        -- tall layout

        ,("tile 1 window fullsize", mytest prop_tile_fullscreen)
        ,("tiles never overlap",    mytest prop_tile_non_overlap)
        ,("split hozizontally",     mytest prop_split_hoziontal)
        ,("split verticalBy",       mytest prop_splitVertically)

        ,("pure layout tall",       mytest prop_purelayout_tall)
        ,("send shrink    tall",    mytest prop_shrink_tall)
        ,("send expand    tall",    mytest prop_expand_tall)
        ,("send incmaster tall",    mytest prop_incmaster_tall)

        -- full layout

        ,("pure layout full",       mytest prop_purelayout_full)
        ,("send message full",      mytest prop_sendmsg_full)
        ,("describe full",          mytest prop_desc_full)

        ,("describe mirror",        mytest prop_desc_mirror)

        -- resize hints
        ,("window hints: inc",      mytest prop_resize_inc)
        ,("window hints: inc all",  mytest prop_resize_inc_extra)
        ,("window hints: max",      mytest prop_resize_max)
        ,("window hints: max all ", mytest prop_resize_max_extra)

        ]
-}


