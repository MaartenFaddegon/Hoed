-- A program with unexpected output.
import CNF
import Debug.Hoed.Pure

main = runOwp properties $ print (prop_negin_correct negin eg)
  where
  properties = [Propositions 
                  [ mkProposition cnfModule "prop_negin_complete"
                     `ofType` BoolProposition
                     `withSignature` [SubjectFunction,Argument 0]
                  , mkProposition cnfModule "prop_negin_sound"
                     `ofType` BoolProposition
                     `withSignature` [SubjectFunction,Argument 0]
                  ] Specify "negin" []
               ]
  cnfModule     = Module "CNF" "../examples/CNF_unsound_demorgan__with_properties/"
