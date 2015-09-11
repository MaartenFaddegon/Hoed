-- A program with unexpected output.
import Digraph
import Debug.Hoed.Pure

-- main = quickcheck prop_idem_negin_sound
main = logOwp "hoed-tests-Prop-t2.graph" properties $ print (prop_assoc1toNdigraph eg)
  where
  properties = [ Property "assoc1toNdigraph" Specifies "Digraph" "prop_assoc1toNdigraph" "../Prop/t2/"
               , Property "mergeAndSortTargets" Specifies "Digraph" "prop_mergeAndSortTargets" "../Prop/t2/"
               , Property "addMissingSources" Specifies "Digraph" "prop_addMissingSources" "../Prop/t2/"
               ]
