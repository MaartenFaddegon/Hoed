-- A program with unexpected output.
import CNF
import Debug.Hoed.Pure

-- main = quickcheck prop_idem_negin_sound
main = logOwp Abort "hoed-tests-Prop-t1.graph" properties $ print (prop_negin_correct eg)
-- main = logOwp "hoed-tests-Prop-t1.graph" properties $ print (negin eg, prop_negin_correct eg)
  where
  properties = [Propositions [(BoolProposition,cnfModule,"prop_negin_complete",[0]), (BoolProposition,cnfModule,"prop_negin_sound",[0])] Specify "negin" []
               ]
  cnfModule  = Module "CNF" "../Prop/t1/"
