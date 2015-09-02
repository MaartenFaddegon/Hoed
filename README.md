# Hoed - A Lightweight Haskell Tracer and Debugger [![Build Status](https://travis-ci.org/MaartenFaddegon/Hoed.svg?branch=master)](https://travis-ci.org/MaartenFaddegon/Hoed)

Hoed is a tracer and debugger for the programming language Haskell.

## Using Hoed

To locate a defect with Hoed you annotate suspected functions and compile as usual. Then you run your program, information about the annotated functions is collected. Finally you connect to a debugging session using a webbrowser.

Let us consider the following program, a defective implementation of a parity function with a test property.

   isOdd :: Int -> Bool
   isOdd = observe "isOdd" isOdd’
   isOdd’ n = isEven (plusOne n)
   
   isEven :: Int -> Bool
   isEven = observe "isEven" isEven’
   isEven’ n = mod2 n == 0
   
   plusOne :: Int -> Int -> Int
   plusOne = observe "plusOne" plusOne’
   plusOne’ n = n + 1
   
   mod2 :: Int -> Int -> Int
   mod2 = observe "mod2" mod2’
   mod2’ n = div n 2
   
   prop_isOdd :: Int -> Bool
   prop_isOdd x = isOdd (2*x+1)

Using the property-based test tool QuickCheck we find the counter example `1` for our property. Hoed can help us determine which function is defective. We annotate the functions `isOdd`, `isEven`, `plusOne` and `mod2` as follows:

   import Debug.Hoed.Pure
   
   isOdd :: Int -> Bool
   isOdd = observe "isOdd" isOdd’
   isOdd’ n = isEven (plusOne n)
   
   isEven :: Int -> Bool
   isEven = observe "isEven" isEven’
   isEven’ n = mod2 n == 0
   
   plusOne :: Int -> Int -> Int
   plusOne = observe "plusOne" plusOne’
   plusOne’ n = n + 1
   
   mod2 :: Int -> Int -> Int
   mod2 = observe "mod2" mod2’
   mod2’ n = div n 2
   
   prop_isOdd :: Int -> Bool
   prop_isOdd x = isOdd (2*x+1)
   
   main :: IO ()
   main = logO "hoed-tests-Pure-t4.graph" $ print (prop_isOdd 1)

After running the program a computation tree is constructed and displayed in a
web browser. You can freely browse this tree to get a better understanding of
your program. If your program misbehaves, you can judge the computation
statements in the tree as 'right' or 'wrong' according to your intention. When
enough statements are judged the debugger tells you the location of the fault
in your code.

## Installation

Hoed is available from Hackage and can be installed with Cabal.

    cabal install Hoed

## Other Tracers

Many of the ideas for Hoed come from the Hat project. Hoed is the Dutch word for a hat. Compared to Hoed, Hat can give more detailed traces. However, Hat requires all modules to be transformed and is therefore not practical for many real-world Haskell programs.

The idea to observe values with local annotations comes from the HOOD project. Unlike Hoed, HOOD does not give relations between observed values. HOOD also requires the programmer to write a class-instance for the type of the value they want to observe. With Hoed these instates can be derived automatically.
