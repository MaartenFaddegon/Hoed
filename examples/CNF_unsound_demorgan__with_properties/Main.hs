-- A program with unexpected output.
import CNF
import Debug.Hoed.Pure

main = runOwp properties $ print (prop_negin_correct negin eg)
  where
  properties = [Propositions [(BoolProposition,cnfModule,"prop_negin_complete",[-1,0]), (BoolProposition,cnfModule,"prop_negin_sound",[-1,0])] Specify "negin" [modQuickCheck]
               ]
  cnfModule     = Module "CNF" "../examples/CNF_unsound_demorgan__with_properties/"
  modQuickCheck = Module "Test.QuickCheck"         ""
