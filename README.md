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

This functional dependency @MonadState s m | m -> s@ means fewer
ambiguities in code that uses this class.  But what happens if we
remove it?  The mtl-unleashed package provides copies of MonadState
and MonadReader with that functional dependency removed, named
MonadStates and MonadReaders.  This allows more flexibility to extract
bits and pieces of state based on type.

These classes allow you to access bits of the State by type, without
knowing exactly what the overall state type is, as in our example above.

    myFunction :: (MonadStates Foo m, MonadStates Bar m) => m r

This will work as long as instances of MonadStates exist for both Foo
and Bar.  The same instances are provided for MonadReaders and
MonadStates as come with MonadReader and MonadState:

    instance Monad m => MonadReaders r (ReaderT r m)
    instance MonadReaders r ((->) r)
    instance MonadReaders r m => MonadReaders r (StateT s m)
    instance (Monoid w, MonadReaders r m) => MonadReaders r (WriterT w m)
    instance MonadReaders r m => MonadReaders r (MaybeT m)
    ...

Additional instances must be added to access parts of a value
(e.g. the individual fields of S, an example using lens operators):

    Data S = S {
      _foo :: Foo,
      _bar :: Bar
    }

    $(makeLenses ''S)

    instance Monad m => MonadStates Foo (StateT S m) where
       getPoly = use foo
       putPoly s = foo .= s

    instance Monad m => MonadReaders Foo (ReaderT S m) where
       askPoly = view foo
       localPoly f action = withReaderT (over foo f) action

    instance Monad m => MonadStates Bar (StateT S m) where
       getPoly = use bar
       putPoly s = bar .= s

    instance Monad m => MonadReaders Bar (ReaderT S m) where
       askPoly = view bar
       localPoly f action = withReaderT (over bar f) action

Now (assuming Foo and Bar are Monoids) you can say

    evalState (return (getPoly, getPoly) :: (Foo, Bar)) (S mempty mempty)

For what we have so far you can use the template haskell utilities included
with lens, specifically makeClassy:

    $(makeClassy ''S)

It is also necessary to provide instances when there are nested
StateT's or ReaderT's, so Haskell knows which one to look in for a
particular type:

    instance MonadStates Foo m => MonadStates Foo (StateT Baz m) where
      getPoly = lift getPoly
      putPoly = lift . putPoly

The implementation of a similar Readers instance shows where we must
resolve an ambiguity that would not bother us with MonadReader:

    instance MonadReaders Foo m => MonadReaders Foo (ReaderT Bar m) where
      askPoly = lift askPoly
      localPoly f action = (askPoly :: m Foo) >>= runReaderT (localPoly f (lift action))
