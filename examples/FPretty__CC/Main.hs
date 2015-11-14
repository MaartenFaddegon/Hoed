import FPretty
import Debug.Hoed.Stk

main = runO $ case pretty 5 d of
  "one\n  two\nthree" -> putStrLn "Success!"
  res                 -> putStrLn $ "Unexpected result:\n" ++ res

  where
  d = group (nest 2 (text "one" <> softline <> text "two"))
      <> group (softline <> text "three")
