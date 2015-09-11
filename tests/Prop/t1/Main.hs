-- A program with unexpected output.
import CNF
import Debug.Hoed.Pure

-- main = quickcheck prop_idem_negin_sound
main = logOwp "hoed-tests-Prop-t1.graph" properties $ print (prop_negin_correct eg)
-- main = logOwp "hoed-tests-Prop-t1.graph" properties $ print (negin eg, prop_negin_correct eg)
  where
  properties = [Propositions [("prop_negin_complete",cnfModule), ("prop_negin_sound",cnfModule)] Specify "negin"]
  cnfModule  = Module "CNF" "../Prop/t1/"
