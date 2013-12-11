base-io-access
==============

An attempt to break up the monolithic IO monad into small, composable classes
that can be used to restrict a function to only having access to, say, functions
to work with the standard pipes, or a function that can access the environment.
The motivation for this library is to allow people to make a stricter contract
than simply "this function does IO", and express through the type system exactly
what IO is being performed.

Implementation
==============

The method used is to make a thinly veiled class breaking up groups of `IO`
actions into smaller sub-groups.  As an example:

```haskell
class Access io => ExitAccess io where
    exitWith'       :: ExitCode -> io a
    exitFailure'    :: io a
    exitSuccess'    :: io a

instance ExitAccess IO where
    exitWith'    = exitWith
    exitFailure' = exitFailure
    exitSuccess' = exitSuccess
```

Here the `Access` class is an empty super class that combines `Monad`, 
`Applicative`, `Functor`, `Typeable1`, and `MonadFix`.  It is the super class 
for all the others in this library.  Maybe eventually I will make it more 
powerful, or add in special support for handling generic `Access` actions, but 
for now it's going to remain very simplistic.

In this example taken from `Access.System.Exit`, we have defined a small class
`ExitAccess` that has three functions, each corresponding to their 
implementation in `System.Exit` but with an extra `'` appended to the name.
By itself this class isn't very interesting, but when combined with (the larger
) `StdIOAccess` class, we could write a function like

```haskell
helpMessage :: (ExitAccess io, StdIOAcess io) => io ()
helpMessage = do
    putStrLn' "Usage:"
    putStrLn' "    mytool [options]"
    putStrLn' "Options:"
    putStrLn' "    --help       Display this help message"
    exitSuccess'

main :: IO ()
main = do
    args <- getArgs
    if "--help" `elem` args
        then helpMessage
        else mainLoop
```

While this is a simple and contrived example, it illustrates that a function's 
capabilities are greatly reduced through this method.  Since we often want to
use the most restrictive type we can in Haskell, this provides an easy way to
achieve this when working with the all-encompassing `IO` monad.

Performance
===========

Since the entire library is based on just using typeclasses, there is no runtime
overhead whatsoever.  The compiler will optimize away the class lookups, and
every function is simply a thin wrapper around its actual implementation in
`base`.
