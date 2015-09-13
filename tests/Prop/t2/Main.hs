-- A program with unexpected output.
import Digraph
import Debug.Hoed.Pure

-- main = quickcheck prop_idem_negin_sound
main = logOwp "hoed-tests-Prop-t2.graph" properties $ print (prop_assoc1toNdigraph eg)
  where
  properties = [ Propositions [(BoolProposition,digraphModule,"prop_assoc1toNdigraph",[1])]    Specify "assoc1toNdigraph" []
               , Propositions [(BoolProposition,digraphModule,"prop_mergeAndSortTargets",[1])] Specify "mergeAndSortTargets" []
               , Propositions [(BoolProposition,digraphModule,"prop_addMissingSources",[1])]   Specify "addMissingSources" []
               ]
  digraphModule = Module "Digraph" "../Prop/t2/"
