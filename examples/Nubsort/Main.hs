import Debug.Hoed.Pure
import Nubsort

main = testOwp ps prop_nub_idempotent [2,1,1]
  where
  ps = [ Propositions [ mkProposition nubsortModule "prop_nub_idempotent" 
                          `ofType` BoolProposition 
                          `withSignature`[Argument 0]
                      , mkProposition nubsortModule "prop_nub_unique"
                          `ofType` BoolProposition 
                          `withSignature`[Argument 0]
                      , mkProposition nubsortModule "prop_nub_complete"
                          `ofType` BoolProposition 
                          `withSignature`[Argument 0]
                      ] PropertiesOf "nub" []
       , Propositions [ mkProposition nubsortModule "prop_nubord'_idempotent"
                          `ofType` QuickCheckProposition
                          `withSignature`[Argument 1, Argument 0]
                      , mkProposition nubsortModule "prop_nubord'_unique"
                          `ofType` QuickCheckProposition
                          `withSignature`[Argument 1, Argument 0]
                      , mkProposition nubsortModule "prop_nubord'_complete"
                          `ofType` QuickCheckProposition
                          `withSignature`[Argument 1, Argument 0]
                      ] PropertiesOf "nubord'" [modQuickCheck]
       , Propositions [ mkProposition nubsortModule "prop_insert_ordered"
                          `ofType` QuickCheckProposition
                          `withSignature`[Argument 1, Argument 0]
                      , mkProposition nubsortModule "prop_insert_complete"
                          `ofType` QuickCheckProposition
                          `withSignature`[Argument 1, Argument 0]
                      ] PropertiesOf "insert" [modQuickCheck]
       ]
  nubsortModule = Module "Nubsort" "../examples/Nubsort/"
  modQuickCheck = Module "Test.QuickCheck" ""
