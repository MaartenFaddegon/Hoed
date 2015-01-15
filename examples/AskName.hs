{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}

import Debug.Hoed

data Person = Person { name :: String, age :: Int, city :: String }
  deriving (Show,Generic)

instance Observable Person

main = runO [] $ emptyPerson *>>= getName >>== getAge >>=* getCity >>= printPerson

emptyPerson :: IO Person
emptyPerson = return (Person "" 0 "")

getName :: Identifier -> (Person -> IO Person, Int)
getName d = let (f,i) = gdmobserve' "getName" d (\p' -> {-# SCC "getName" #-} getName' p')
            in (f, i)
getName' p = do putStr "name: "; getLine >>= \x -> return (p{ name = x })

getAge :: Identifier -> (Person -> IO Person, Int)
getAge d = let (f,i) = gdmobserve' "getAge" d (\p' -> {-# SCC "getAge" #-} getAge' p')
           in (f, i)
getAge' p = do putStr "age: "; readLn >>= \x -> return (p{ age = x })

getCity :: Identifier -> (Person -> IO Person, Int)
getCity d = let (f,i) = gdmobserve' "getCity" d (\p' -> {-# SCC "getCity" #-} getCity' p')
            in (f, i)
getCity' p = do putStr "city: "; getLine >>= \x -> return (p{ city = x })

printPerson :: Person -> IO ()
printPerson = print
