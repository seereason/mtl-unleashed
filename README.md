# mtl-unleashed
MonadReader and MonadState without the functional dependencies

  Classes MonadState and MonadReader without the functional
  dependency from the monad to the contained type.  This allows
  more flexibility to extract bits and pieces of state based on
  type, but increases ambiguities that need to be resolved with
  extra type signatures.
  
  These classes allow you to access bits of the State by type,
  without knowing exactly what the overall state type is.  For
  example:
  
      typeGraphEdges :: (DsMonad m,
                         MonadReader TypeGraph m,
                         MonadStates Foo m,
                         MonadStates Bar m) => ...
  
  This will work as long as the two MonadStates instances exist for
  whatever the actual State type is:
  
      Data S = S {
       _foo :: Foo,
       _bar :: Bar
      }
  
      $(makeLenses ''S)
  
      instance Monad m => MonadStates Foo (StateT S m) where
         get = use foo
         modify f = foo %= f

      instance Monad m => MonadStates Bar (StateT S m) where
         get = use bar
         modify f = bar %= f
  
  Now you can say
  
      evalState (return (getState, getState) :: (Foo, Bar)) (S foo0, bar0)
  
  You can even write instances to reach down into nested StateT's as
  long as you know the exact type you are reaching down through, in
  this case StateT Bar:
  
      instance MonadStates Foo m => MonadStates Foo (StateT Baz m) where
        get = lift get
        modify f = lift $ modify f
