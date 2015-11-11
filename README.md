# mtl-unleashed
MonadReader and MonadState without the functional dependencies

Have you ever wanted to say

    myFunction :: (MonadState Foo m, MonadState Bar m) => m r
    myFunction = do
      (foo :: Foo) <- get
      (bar :: Bar) <- get
      return $ myPureFn foo bar

that is, declare two different MonadState values (Foo and Bar) for a
single monad m?  If so, you almost certainly saw something like this:

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
and MonadReader (named MonadStates and MonadReaders) with that
functional dependency removed.  This allows more flexibility to
extract bits and pieces of state based on type, at the expense of
occasionally having to insert a disambiguating signature.

These classes allow you to access bits of the State by type, without
knowing exactly what the overall state type is, as in our example above.

    myFunction :: (MonadStates Foo m, MonadStates Bar m) => m r

This will work as long as the caller's monad m has instances of
MonadStates for both Foo and Bar.  The same set of default instances
are provided for MonadReaders and MonadStates as come with MonadReader
and MonadState:

    instance Monad m => MonadReaders r (ReaderT r m)
    instance MonadReaders r ((->) r)
    instance MonadReaders r m => MonadReaders r (StateT s m)
    instance (Monoid w, MonadReaders r m) => MonadReaders r (WriterT w m)
    instance MonadReaders r m => MonadReaders r (MaybeT m)
    ...

There are two cases where additional instances must be supplied: (1)
when you want to access a piece of a value, or (2) when you want to
reach down through nested ReaderT or StateT monads to get to a value.
As an example of (1), here we create MonadState instances for the
types Foo and Bar by supplying instances that extract them from a type
S (using lens operators):

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

Now you can say

    evalState (return (getPoly, getPoly) :: (Foo, Bar)) (S mempty mempty)

If we have nested StateT's or ReaderT's we need to supply an instance
to determine how to lift the buried one to get at a type.  Because we
know there is a Foo in m (and presumably none in Baz) we can get at it
using a lift:

    instance MonadStates Foo m => MonadStates Foo (StateT Baz m) where
      getPoly = lift getPoly
      putPoly = lift . putPoly

The implementation of a similar instance for ReaderT shows where we
must resolve an ambiguity that would not bother us with MonadReader:

    instance MonadReaders Foo m => MonadReaders Foo (ReaderT Bar m) where
      askPoly = lift askPoly
      localPoly f action = askPoly >>= \(foo :: Foo) -> runReaderT (localPoly f (lift action)) foo
