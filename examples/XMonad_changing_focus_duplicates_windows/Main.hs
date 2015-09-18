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

myStackSet :: T
myStackSet = StackSet {current = Screen {workspace = Workspace {tag = NonNegative 2, layout = -2, stack = Just (Stack {focus = 'c', up = "", down = "z"})}, screen = 2, screenDetail = 1}, visible = [Screen {workspace = Workspace {tag = NonNegative 0, layout = -2, stack = Just (Stack {focus = 'd', up = "", down = ""})}, screen = 1, screenDetail = -2},Screen {workspace = Workspace {tag = NonNegative 3, layout = -2, stack = Just (Stack {focus = 'v', up = "", down = ""})}, screen = 3, screenDetail = -1},Screen {workspace = Workspace {tag = NonNegative 4, layout = -2, stack = Just (Stack {focus = 'w', up = "", down = "i"})}, screen = 0, screenDetail = -2}], hidden = [Workspace {tag = NonNegative 1, layout = -2, stack = Just (Stack {focus = 'n', up = "", down = ""})},Workspace {tag = NonNegative 0, layout = -2, stack = Nothing},Workspace {tag = NonNegative 4, layout = -2, stack = Nothing}], floating = M.fromList []}



{- Running all tests
main = do
    args <- fmap (drop 1) getArgs
    let n = if null args then 100 else read (head args)
    (results, passed) <- liftM unzip $ mapM (\(s,a) -> printf "%-40s: " s >> a n) tests
    printf "Passed %d tests!\n" (sum passed)
    when (not . and $ results) $ fail "Not all tests passed!"
-}

main :: IO ()
main = runO $ do
  g <- newStdGen
  print . fromJust . ok . (generate 1 g) . evaluate $  prop_shift_win_I 1 'd' myStackSet
  where
  module_Properties = Module "Properties"              "../examples/XMonad_changing_focus_duplicates_windows/"
  module_StackSet   = Module "XMonad.StackSet"         "../examples/XMonad_changing_focus_duplicates_windows/"
  module_QuickCheck = Module "Test.QuickCheck"         "../examples/XMonad_changing_focus_duplicates_windows/"
  module_Map        = Module "qualified Data.Map as M" ""
  module_Random     = Module "System.Random"           ""
  module_Maybe      = Module "Data.Maybe"              ""

  tests =
      [("shiftMaster id on focus", mytest prop_shift_master_focus)
      ,("shiftWin: invariant" , mytest prop_shift_win_I)
      ,("shiftWin is shift on focus" , mytest prop_shift_win_focus)
      ,("shiftWin fix current" , mytest prop_shift_win_fix_current)
      ]
