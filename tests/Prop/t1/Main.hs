-- A program with unexpected output.
import CNF
import Debug.Hoed.Pure

-- main = quickcheck prop_idem_negin_sound
main = logOwp Bottom "hoed-tests-Prop-t1.graph" properties $ print (prop_negin_correct eg)
-- main = logOwp "hoed-tests-Prop-t1.graph" properties $ print (negin eg, prop_negin_correct eg)
  where
  properties = [Propositions 
                 [ mkProposition cnfModule "prop_negin_complete"
                   `ofType` BoolProposition `withSignature` [Argument 0]
                 , mkProposition cnfModule "prop_negin_sound"
                   `ofType` BoolProposition `withSignature` [Argument 0]
                 ] Specify "negin" []
               ]
  cnfModule  = Module "CNF" "../Prop/t1/"
