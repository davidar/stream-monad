# Simple, Fair and Terminating Backtracking Monad Transformer

This Haskell library provides an implementation of the MonadPlus type
class that enumerates results of a non-deterministic computation by
interleaving subcomputations in a way that has usually much better
memory performance than other strategies with the same termination
properties.

It also terminates in many cases where the fair conjunction and
interleaving operators provided by LogicT fail to do so, allowing it
to safely provide fairness by default.

More information is available on the [author's website][FBackTrackT].

This package aims to be a drop-in replacement for the unmaintained
`stream-monad` package, in addition to providing much of the same
functionality as the `logict` package.

[FBackTrackT]: http://okmij.org/ftp/Computation/monads.html#fair-bt-stream

