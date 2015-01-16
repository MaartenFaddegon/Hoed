-- This is a copy of AskName.hs, annotated with Hood instead of Hoed. It shows
-- that with the trace produced by Hood it is not easy to see that an
-- exception was thrown in getAge. The only way we can tell seems to be the
-- absence of "<IO>" in the result of the getAge computation statement.
--
--              $ ./AskName-hood 
--              aaa
--              def
--              [Escaping Exception in Code : user error (Prelude.readIO: no parse)]
--              
--              -- getAge
--                { \ _  -> _
--                }
--              -- getName
--                { \ _  -> <IO> _
--                }
--              -- main
--                _



import Debug.Hood.Observe

data Person = Person { name :: String, age :: Int, city :: String } deriving Show

instance Observable Person where
  observer (Person n a c) = send "Person" (return Person << n << a << c)

main = runO $ observe "main" (emptyPerson >>= getName >>= getAge >>= getCity >>= print)

emptyPerson :: IO Person
emptyPerson = return (Person "" 0 "")

getName :: Person -> IO Person
getName = observe "getName" (\p' -> getName' p')
getName' p = getLine >>= \x -> return (p{ name = x })

getAge :: Person -> IO Person
getAge = observe "getAge" (\p' -> getAge' p')
getAge' p = readLn >>= \x -> return (p{ age = x })

getCity :: Person -> IO Person
getCity = observe "getCity" (\p' -> getCity' p')
getCity' p = getLine >>= \x -> return (p{ city = x })
