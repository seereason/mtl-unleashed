# mtl-unleashed
MonadReader and MonadState without the functional dependencies

Have you ever wanted to say

    myFunction :: (MonadState Foo m, MonadState Bar m) => m r
    myFunction = do
      (foo :: Foo) <- get
      (bar :: Bar) <- get
      return $ myPureFn foo bar

If so, you almost certainly saw something like this:

    Couldn't match type ‘Foo’ with ‘Bar’
    arising from a functional dependency between constraints:
      ‘MonadState Bar m’
        arising from the type signature for
                       myFunction :: (MonadState Foo m, MonadState Bar m) => m r
      ‘MonadState Foo m’
        arising from the type signature for
                       myFunction :: (MonadState Foo m, MonadState Bar m) => m r

What happens if we remove that functional dependency?  The
mtl-unleashed package provides copies of MonadState and MonadReader
with that functional dependency removed, named MonadStates and
MonadReaders.  This allows more flexibility to extract bits and pieces
of state based on type, but increases ambiguities that need to be
resolved with extra type signatures.

These classes allow you to access bits of the State by type, without
knowing exactly what the overall state type is, as in our example above.

    typeGraphEdges :: (DsMonad m,
                       MonadReader TypeGraph m,
                       MonadStates Foo m,
                       MonadStates Bar m) => ...

This will work as long as the two MonadStates instances exist for
whatever the actual State type is:  (using the lens package here)

    Data S = S {
      _foo :: Foo,
      _bar :: Bar
    }

    $(makeLenses ''S)

    instance Monad m => MonadStates Foo (StateT S m) where
       get = use foo
       put s = foo .= s

    instance Monad m => MonadStates Bar (StateT S m) where
       get = use bar
       put s = bar .= s

Now you can say

    evalState (return (get, get) :: (Foo, Bar)) (S mempty mempty)

You can even write instances to reach down into nested StateT's as
long as you know the exact type you are reaching down through, in
this case StateT Bar:

    instance MonadStates Foo m => MonadStates Foo (StateT Baz m) where
      get = lift get
      put s = lift $ put s

    instance MonadReaders Foo m => MonadReaders Foo (ReaderT Bar m) where
      ask = lift ask
      local f action = ask >>= \(r :: Bar) -> runReaderT (local f (lift action)) r
