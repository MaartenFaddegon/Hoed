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
myStackSet = StackSet {current = Screen {workspace = Workspace {tag = NonNegative 5, layout = 0, stack = Nothing}, screen = 5, screenDetail = 0}, visible = [Screen {workspace = Workspace {tag = NonNegative 0, layout = 0, stack = Nothing}, screen = 1, screenDetail = 0},Screen {workspace = Workspace {tag = NonNegative 2, layout = 0, stack = Nothing}, screen = 2, screenDetail = 0},Screen {workspace = Workspace {tag = NonNegative 3, layout = 0, stack = Nothing}, screen = 3, screenDetail = 0},Screen {workspace = Workspace {tag = NonNegative 4, layout = 0, stack = Nothing}, screen = 4, screenDetail = 0},Screen {workspace = Workspace {tag = NonNegative 6, layout = 0, stack = Nothing}, screen = 6, screenDetail = 0},Screen {workspace = Workspace {tag = NonNegative 7, layout = 0, stack = Nothing}, screen = 0, screenDetail = 0}], hidden = [Workspace {tag = NonNegative 1, layout = 0, stack = Nothing}], floating = M.fromList []}

main :: IO ()
main = runOwp propositions $ do
  g <- newStdGen
  print . fromJust . ok . (generate 1 g) . evaluate $  prop_greedyView_local greedyView myStackSet 0
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
                          `withSignature` [SubjectFunction, Argument 1, Argument 0]
                          `withTestGen` TestGenLegacyQuickCheck
                       , mkProposition module_Properties "prop_greedyView_local"
                          `ofType` LegacyQuickCheckProposition
                          `withSignature` [SubjectFunction, Argument 1, Argument 0]
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
