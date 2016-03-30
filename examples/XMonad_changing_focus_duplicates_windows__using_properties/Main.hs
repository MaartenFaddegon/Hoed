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


main :: IO ()
main = runOwp propositions $ do
  g <- newStdGen
  print . fromJust . ok . (generate 1 g) . evaluate $  prop_shift_win_I shiftWin 1 'd' myStackSet
  where
    propositions =
        [ Propositions [mkProposition module_Properties "prop_shift_win_I"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [SubjectFunction, Argument 0, Argument 1, Argument 2]
                          `withTestGen` TestGenLegacyQuickCheck
                       ]
                       PropertiesOf "shiftWin" [module_StackSet, module_QuickCheck, module_Map, module_Random, module_Maybe]
        , Propositions [mkProposition module_Properties "prop_focus_all_l"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [Argument 0]
                          `withTestGen` TestGenLegacyQuickCheck
                      ,mkProposition module_Properties "prop_focus_all_l_weak"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [Argument 0]
                          `withTestGen` TestGenLegacyQuickCheck
                      ]
                      PropertiesOf "focusUp" [module_StackSet, module_QuickCheck, module_Map, module_Random, module_Maybe]

        , Propositions [ mkProposition module_Properties "prop_insert_duplicate_weak"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [Argument 0, Argument 1]
                          `withTestGen` TestGenLegacyQuickCheck
                       ] 
                       PropertiesOf "insertUp" [module_StackSet, module_QuickCheck, module_Map, module_Random, module_Maybe]

        , Propositions [ mkProposition module_Properties "prop_view_reversible"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [Argument 0, Argument 1]
                          `withTestGen` TestGenLegacyQuickCheck
                         , mkProposition module_Properties "prop_view_I"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [SubjectFunction, Argument 0, Argument 1]
                          `withTestGen` TestGenLegacyQuickCheck
                         , mkProposition module_Properties "prop_view_current"
                            `ofType` LegacyQuickCheckProposition
                            `withSignature` [Argument 1, Argument 0]
                            `withTestGen` TestGenLegacyQuickCheck
                         , mkProposition module_Properties "prop_view_idem"
                            `ofType` LegacyQuickCheckProposition
                            `withSignature` [Argument 1, Argument 0]
                            `withTestGen` TestGenLegacyQuickCheck
                         , mkProposition module_Properties "prop_view_local"
                            `ofType` LegacyQuickCheckProposition
                            `withSignature` [Argument 1, Argument 0]
                            `withTestGen` TestGenLegacyQuickCheck
                       ] 
                       PropertiesOf "view" [module_StackSet, module_QuickCheck, module_Map, module_Random, module_Maybe]

        , Propositions [ mkProposition module_Properties "prop_greedyView_reversible"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [Argument 0, Argument 1]
                          `withTestGen` TestGenLegacyQuickCheck
                       , mkProposition module_Properties "prop_greedyView_idem"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [Argument 1, Argument 0]
                          `withTestGen` TestGenLegacyQuickCheck
                       ] 
                       PropertiesOf "greedyView" [module_StackSet, module_QuickCheck, module_Map, module_Random, module_Maybe]
        , Propositions [ mkProposition module_Properties "prop_findIndex"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [Argument 1]
                          `withTestGen` TestGenLegacyQuickCheck
                       ]
                       PropertiesOf "findTag" [module_StackSet, module_QuickCheck, module_Map, module_Random, module_Maybe]

        ]

    module_Properties = Module "Properties"              "../examples/XMonad_changing_focus_duplicates_windows__using_properties/"
    module_StackSet   = Module "XMonad.StackSet"         "../examples/XMonad_changing_focus_duplicates_windows__using_properties/"
    module_QuickCheck = Module "Test.QuickCheck"         "../examples/XMonad_changing_focus_duplicates_windows__using_properties/"
    module_Map        = Module "qualified Data.Map as M" ""
    module_Random     = Module "System.Random"           ""
    module_Maybe      = Module "Data.Maybe"              ""
