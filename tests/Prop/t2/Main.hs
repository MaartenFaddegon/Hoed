-- A program with unexpected output.
import Digraph
import Debug.Hoed.Pure

-- main = quickcheck prop_idem_negin_sound
main = logOwp Bottom "hoed-tests-Prop-t2.graph" properties $ print (prop_assoc1toNdigraph eg)
  where
  properties = [ Propositions [mkProposition digraphModule "prop_assoc1toNdigraph"    
                                 `ofType` BoolProposition `withSignature` [Argument 0]] Specify "assoc1toNdigraph" []
               , Propositions [mkProposition digraphModule "prop_mergeAndSortTargets"
                                 `ofType` BoolProposition `withSignature` [Argument 0]] Specify "mergeAndSortTargets" []
               , Propositions [mkProposition digraphModule "prop_addMissingSources" 
                                 `ofType` BoolProposition `withSignature` [Argument 0]] Specify "addMissingSources" []
               ]
  digraphModule = Module "Digraph" "../Prop/t2/"
