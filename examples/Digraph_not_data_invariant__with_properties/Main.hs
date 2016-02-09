-- A program with unexpected output.
import Digraph
import Debug.Hoed.Pure

main = runOwp properties $ print (prop_assoc1toNdigraph eg)
  where
  properties = [ Propositions [(BoolProposition,digraphModule,"prop_assoc1toNdigraph",[Argument 0])]
                   Specify "assoc1toNdigraph" []
               , Propositions [(BoolProposition,digraphModule,"prop_mergeAndSortTargets",[Argument 0])]
                   Specify "mergeAndSortTargets" []
               , Propositions [(BoolProposition,digraphModule,"prop_addMissingSources",[Argument 0])]
                   Specify "addMissingSources" []
               ]
  digraphModule = Module "Digraph" "../examples/Digraph_not_data_invariant__with_properties/"
