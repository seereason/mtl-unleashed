{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens

type Foo = Int
type Bar = Char

data T
    = T { _foo :: Foo
        , _bar :: Bar }

$(makeClassy ''T)
