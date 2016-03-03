import Properties
import Debug.Hoed.NoTrace
import System.Environment
import Text.Printf
import Control.Monad
import System.Random
import Test.QuickCheck
import Data.Maybe
import XMonad.StackSet
import qualified Data.Map as M

myStackSet :: T
myStackSet = StackSet {current = Screen {workspace = Workspace {tag = NonNegative 2, layout = -2, stack = Just (Stack {focus = 'c', up = "", down = "z"})}, screen = 2, screenDetail = 1}, visible = [Screen {workspace = Workspace {tag = NonNegative 0, layout = -2, stack = Just (Stack {focus = 'd', up = "", down = ""})}, screen = 1, screenDetail = -2},Screen {workspace = Workspace {tag = NonNegative 3, layout = -2, stack = Just (Stack {focus = 'v', up = "", down = ""})}, screen = 3, screenDetail = -1},Screen {workspace = Workspace {tag = NonNegative 4, layout = -2, stack = Just (Stack {focus = 'w', up = "", down = "i"})}, screen = 0, screenDetail = -2}], hidden = [Workspace {tag = NonNegative 1, layout = -2, stack = Just (Stack {focus = 'n', up = "", down = ""})},Workspace {tag = NonNegative 0, layout = -2, stack = Nothing},Workspace {tag = NonNegative 4, layout = -2, stack = Nothing}], floating = M.fromList []}



main = do
    args <- fmap (drop 1) getArgs
    let n = if null args then 100 else read (head args)
    (results, passed) <- liftM unzip $ mapM (\(s,a) -> printf "%-40s: " s >> a n) tests
    printf "Passed %d tests!\n" (sum passed)
    when (not . and $ results) $ fail "Not all tests passed!"

    where

    tests = zipWith (\(name,test) number -> (show number ++ ": " ++ name,test)) tests' [1..]

    tests' =
        [("prop_invariant" , mytest prop_invariant)

        ,("prop_empty_I"    , mytest prop_empty_I)
        ,("prop_empty"      , mytest prop_empty)
        ,("prop_empty_current"     , mytest prop_empty_current)
        ,("prop_member_empty"      , mytest prop_member_empty)

        ,("prop_view_I"    , mytest prop_view_I)
        ,("prop_view_current"   , mytest prop_view_current)
        ,("prop_view_idem"     , mytest prop_view_idem)
        ,("prop_view_reversible"    , mytest prop_view_reversible)
        ,("prop_view_local"       , mytest prop_view_local)

        ,("prop_view_greedyView_I"    , mytest prop_greedyView_I)
        ,("prop_greedyView_current"   , mytest prop_greedyView_current)
        ,("prop_greedyView_current_id"   ,   mytest prop_greedyView_current_id)
        ,("prop_greedyView_idem"     , mytest prop_greedyView_idem)
        ,("prop_greedyView_reversible"     , mytest prop_greedyView_reversible)
        ,("prop_greedyView_local"       , mytest prop_greedyView_local)

        ,("prop_member_peek"        , mytest prop_member_peek)

        ,("prop_index_length"        , mytest prop_index_length)

        ,("prop_focusUp_I", mytest prop_focusUp_I)
        ,("prop_focusMaster_I", mytest prop_focusMaster_I)
        ,("prop_focusDown_I", mytest prop_focusDown_I)
        ,("prop_focus_I", mytest prop_focus_I)
        ,("prop_focus_left_master"   , mytest prop_focus_left_master)
        ,("prop_focus_right_master"  , mytest prop_focus_right_master)
        ,("prop_focus_master_master"  , mytest prop_focus_master_master)
        ,("prop_focusWindow_master"  , mytest prop_focusWindow_master)
        ,("prop_focus_left"    , mytest prop_focus_left)
        ,("prop_focus_right"    , mytest prop_focus_right)
        ,("prop_focus_all_l"    , mytest prop_focus_all_l)
        ,("prop_focus_all_r"    , mytest prop_focus_all_r)
        ,("prop_focus_down_local"      , mytest prop_focus_down_local)
        ,("prop_focus_up_local"      , mytest prop_focus_up_local)
        ,("prop_focus_master_local"      , mytest prop_focus_master_local)
        ,("prop_focusMaster_idem"  , mytest prop_focusMaster_idem)

        ,("prop_focusWindow_local", mytest prop_focusWindow_local)
        ,("prop_focusWindow_works"   , mytest prop_focusWindow_works)
        ,("prop_focusWindow_identity", mytest prop_focusWindow_identity)

        ,("prop_findIndex"           , mytest prop_findIndex)
        ,("prop_allWindowsMember"   , mytest prop_allWindowsMember)
        ,("prop_currentTag"          , mytest prop_currentTag)

        ,("prop_insertUp_I"   , mytest prop_insertUp_I)
        ,("prop_insert_empty/new"          , mytest prop_insert_empty)
        ,("prop_insert_idem", mytest prop_insert_idem)
        ,("prop_insert_delete", mytest prop_insert_delete)
        ,("prop_insert_local"     , mytest prop_insert_local)
        ,("prop_insert_duplicate"   , mytest prop_insert_duplicate)
        ,("prop_insert_peek"        , mytest prop_insert_peek)
        ,("prop_size_insert"         , mytest prop_size_insert)

        ,("prop_delete_I"   , mytest prop_delete_I)
        ,("prop_empty"        , mytest prop_empty)
        ,("prop_delete"       , mytest prop_delete)
        ,("prop_delete_insert", mytest prop_delete_insert)
        ,("prop_delete_local"     , mytest prop_delete_local)
        ,("prop_delete_focus"        , mytest prop_delete_focus)
        ,("prop_delete_focus_end", mytest prop_delete_focus_end)
        ,("prop_delete_focus_not_end", mytest prop_delete_focus_not_end)

        ,("prop_filter_order", mytest prop_filter_order)

        ,("prop_swap_master_I", mytest prop_swap_master_I)
        ,("prop_swap_left_I" , mytest prop_swap_left_I)
        ,("prop_swap_right_I", mytest prop_swap_right_I)
        ,("prop_swap_master_focus", mytest prop_swap_master_focus)
        ,("prop_swap_left_focus", mytest prop_swap_left_focus)
        ,("prop_swap_right_focus", mytest prop_swap_right_focus)
        ,("prop_swap_master_idempotent", mytest prop_swap_master_idempotent)
        ,("prop_swap_all_l"     , mytest prop_swap_all_l)
        ,("prop_swap_all_r"     , mytest prop_swap_all_r)
        ,("prop_swap_master_local" , mytest prop_swap_master_local)
        ,("prop_swap_left_local"   , mytest prop_swap_left_local)
        ,("prop_swap_right_local"  , mytest prop_swap_right_local)

        ,("prop_shift_master_focus", mytest prop_shift_master_focus)
        ,("prop_shift_master_local", mytest prop_shift_master_local)
        ,("prop_shift_master_idempotent", mytest prop_shift_master_idempotent)
        ,("prop_shift_master_ordering", mytest prop_shift_master_ordering)

        ,("prop_shift_I"    , mytest prop_shift_I)
        ,("prop_shift_reversible" , mytest prop_shift_reversible)
        ,("prop_shift_win_I" , mytest prop_shift_win_I)
        ,("prop_shift_win_focus" , mytest prop_shift_win_focus)
        ,("prop_shift_win_fix_current" , mytest prop_shift_win_fix_current)

        ,("prop_float_reversible" , mytest prop_float_reversible)
        ,("prop_float_geometry" , mytest prop_float_geometry)
        ,("prop_float_delete", mytest prop_float_delete)
        ,("prop_screens", mytest prop_screens)

        ,("prop_differentiate", mytest prop_differentiate)
        ,("prop_lookup_current", mytest prop_lookup_current)
        ,("prop_lookup_visible", mytest prop_lookup_visible)
        ,("prop_screens_works",      mytest prop_screens_works)
        ,("prop_rename1",     mytest prop_rename1)
        ,("prop_ensure",     mytest prop_ensure)
        ,("prop_ensure_append",     mytest prop_ensure_append)

        ,("prop_mapWorkspaceId", mytest prop_mapWorkspaceId)
        ,("prop_mapWorkspaceInverse", mytest prop_mapWorkspaceInverse)
        ,("prop_mapLayoutId", mytest prop_mapLayoutId)
        ,("prop_mapLayoutInverse", mytest prop_mapLayoutInverse)

        -- testing for failure:
        ,("prop_abort",            mytest prop_abort)
        ,("prop_new_abort",   mytest prop_new_abort)
        ,("prop_shift_win_indentity",      mytest prop_shift_win_indentity)

        -- tall layout

        ,("prop_tile_fullscreen", mytest prop_tile_fullscreen)
        ,("prop_tile_non_overlap",    mytest prop_tile_non_overlap)
        ,("prop_split_hoziontal",     mytest prop_split_hoziontal)
        ,("prop_splitVertically",       mytest prop_splitVertically)

        ,("prop_purelayout_tall",       mytest prop_purelayout_tall)
        ,("prop_shrink_tall",    mytest prop_shrink_tall)
        ,("prop_expand_tall",    mytest prop_expand_tall)
        ,("prop_incmaster_tall",    mytest prop_incmaster_tall)

        -- full layout

        ,("prop_purelayout_full",       mytest prop_purelayout_full)
        ,("prop_sendmsg_full",      mytest prop_sendmsg_full)
        ,("prop_desc_full",          mytest prop_desc_full)

        ,("prop_desc_mirror",        mytest prop_desc_mirror)

        -- resize hints
        ,("prop_resize_inc",      mytest prop_resize_inc)
        ,("prop_resize_inc_extra",  mytest prop_resize_inc_extra)
        ,("prop_resize_max",      mytest prop_resize_max)
        ,("prop_resize_max_extra", mytest prop_resize_max_extra)

        ]
