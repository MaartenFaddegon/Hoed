-- A program with unexpected output.
import Digraph
import Debug.Hoed.Pure

main = runOwp properties $ print (prop_assoc1toNdigraph eg)
  where
  properties = [ Propositions [mkProposition digraphModule "prop_assoc1toNdigraph" `ofType` BoolProposition `withSignature` [Argument 0]]
                   Specify "assoc1toNdigraph" []
               , Propositions [mkProposition digraphModule "prop_mergeAndSortTargets" `ofType` BoolProposition `withSignature` [Argument 0]]
                   Specify "mergeAndSortTargets" []
               , Propositions [mkProposition digraphModule "prop_addMissingSources" `ofType` BoolProposition `withSignature` [Argument 0]]
                   Specify "addMissingSources" []
               ]
  digraphModule = Module "Digraph" "../examples/Digraph_not_data_invariant__with_properties/"
