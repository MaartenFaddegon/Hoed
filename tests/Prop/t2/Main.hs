-- A program with unexpected output.
import Digraph
import Debug.Hoed.Pure

-- main = quickcheck prop_idem_negin_sound
main = logOwp "hoed-tests-Prop-t2.graph" properties $ print (prop_assoc1toNdigraph eg)
  where
  properties = [ Propositions [("prop_assoc1toNdigraph",digraphModule)]    Specify "assoc1toNdigraph"
               , Propositions [("prop_mergeAndSortTargets",digraphModule)] Specify "mergeAndSortTargets"
               , Propositions [("prop_addMissingSources",digraphModule)]   Specify "addMissingSources"
               ]
  digraphModule = Module "Digraph" "../Prop/t2/"
