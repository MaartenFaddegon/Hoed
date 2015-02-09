# Hoed - A Lightweight Haskell Tracer and Debugger

Hoed is a tracer and debugger for the programming language Haskell. You can
trace a program by annotating functions in suspected modules and linking your
program against standard profiling libraries. 

After running the program a computation tree is constructed and displayed in a
web browser. You can freely browse this tree to get a better understanding of
your program. If your program misbehaves, you can judge the computation
statements in the tree as 'right' or 'wrong' according to your intention. When
enough statements are judged the debugger tells you the location of the fault
in your code.

## Installation

Hoed is available from Hackage and can be installed with Cabal.

Because Hoed relies on profiling information to construct a computation tree,
it is important to enable profiling before installing Hoed. Edit the main
Cabal config file ($HOME/cabal/config) and make sure profiling is enabled:

    library-profiling: True
    executable-profiling: True

You are now ready to download and install Hoed from Hackage:

    cabal install Hoed

If you installed libraries for your project before and profiling was disabled
you need to re-install these libraries before using Hoed.

## Example

TODO: do an example here.

## Other Tracers

Many of the ideas for Hoed come from the Hat project. Hoed is the Dutch word
for a hat (as in, a thing to keep your head warm). Compared to Hoed, Hat can
give more detailed traces. However, Hat requires all modules to be transformed
and is therefore not practical for most programs.

The idea to observe values with local annotations comes from the HOOD project.
Unlike Hoed, HOOD does not give relations between observed values. HOOD also
requires the programmer to write a class-instance for the type of the value
they want to observe. With Hoed these instates can be derived automatically.
