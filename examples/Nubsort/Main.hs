import Debug.Hoed.Pure
import Nubsort

main = testOwp ps prop_nub_idempotent [2,1,1]
  where
  ps = [ Propositions [ (BoolProposition,nubsortModule,"prop_nub_idempotent",[Argument 0])
                      , (BoolProposition,nubsortModule,"prop_nub_unique",[Argument 0])
                      , (BoolProposition,nubsortModule,"prop_nub_complete",[Argument 0])
                      ] PropertiesOf "nub" []
       , Propositions [ (QuickCheckProposition,nubsortModule,"prop_nubord'_idempotent",[Argument 1,Argument 0])
                      , (QuickCheckProposition,nubsortModule,"prop_nubord'_unique",[Argument 1,Argument 0])
                      , (QuickCheckProposition,nubsortModule,"prop_nubord'_complete",[Argument 1,Argument 0])
                      ] PropertiesOf "nubord'" [modQuickCheck]
       , Propositions [ (QuickCheckProposition,nubsortModule,"prop_insert_ordered",[Argument 1,Argument 0])
                      , (QuickCheckProposition,nubsortModule,"prop_insert_complete",[Argument 1,Argument 0])
                      ] PropertiesOf "insert" [modQuickCheck]
       ]
  nubsortModule = Module "Nubsort" "../examples/Nubsort/"
  modQuickCheck = Module "Test.QuickCheck" ""
