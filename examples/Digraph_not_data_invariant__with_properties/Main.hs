-- A program with unexpected output.
import Digraph
import Debug.Hoed.Pure

main = runOwp properties $ print (prop_assoc1toNdigraph eg)
  where
  properties = [ Propositions [(BoolProposition,digraphModule,"prop_assoc1toNdigraph",[0])]    Specify "assoc1toNdigraph" [modQuickCheck]
               , Propositions [(BoolProposition,digraphModule,"prop_mergeAndSortTargets",[0])] Specify "mergeAndSortTargets" [modQuickCheck]
               , Propositions [(BoolProposition,digraphModule,"prop_addMissingSources",[0])]   Specify "addMissingSources" [modQuickCheck]
               ]
  digraphModule = Module "Digraph" "../examples/Digraph_not_data_invariant__with_properties/"
  modQuickCheck = Module "Test.QuickCheck" ""
