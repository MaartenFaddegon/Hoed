import Debug.Hoed.Pure
import Nubsort

main = testOwp ps prop_nub_idempotent [2,1,1]
  where
  ps = [ Propositions [ (BoolProposition,nubsortModule,"prop_nub_idempotent",[0])
                      , (BoolProposition,nubsortModule,"prop_nub_unique",[0])
                      , (BoolProposition,nubsortModule,"prop_nub_complete",[0])
                      ] PropertiesOf "nub" []
       , Propositions [ (QuickCheckProposition,nubsortModule,"prop_nubord'_idempotent",[1,0])
                      , (QuickCheckProposition,nubsortModule,"prop_nubord'_unique",[1,0])
                      , (QuickCheckProposition,nubsortModule,"prop_nubord'_complete",[1,0])
                      ] PropertiesOf "nubord'" [modQuickCheck]
       , Propositions [ (QuickCheckProposition,nubsortModule,"prop_insert_ordered",[1,0])
                      , (QuickCheckProposition,nubsortModule,"prop_insert_complete",[1,0])
                      ] PropertiesOf "insert" [modQuickCheck]
       ]
  nubsortModule = Module "Nubsort" "../examples/Nubsort/"
  modQuickCheck = Module "Test.QuickCheck" ""
