{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

import Debug.Hoed

data Person = Person { name :: String, age :: Int, city :: String }
  deriving (Show,Generic)

instance Observable Person

main = runO $ observe "main" 
        ({-# SCC "main" #-} emptyPerson *>>= getName >>== getAge >>=* getCity >>= print)

emptyPerson :: IO Person
emptyPerson = return (Person "" 0 "")

getName :: Identifier -> (Person -> IO Person, Int)
getName d = let (f,i) = observe' "getName" d (\p' -> {-# SCC "getName" #-} getName' p')
            in (f, i)
getName' p = getLine >>= \x -> return (p{ name = x })

getAge :: Identifier -> (Person -> IO Person, Int)
getAge d = let (f,i) = observe' "getAge" d (\p' -> {-# SCC "getAge" #-} getAge' p')
           in (f, i)
getAge' p = readLn >>= \x -> return (p{ age = x })

getCity :: Identifier -> (Person -> IO Person, Int)
getCity d = let (f,i) = observe' "getCity" d (\p' -> {-# SCC "getCity" #-} getCity' p')
            in (f, i)
getCity' p = getLine >>= \x -> return (p{ city = x })
