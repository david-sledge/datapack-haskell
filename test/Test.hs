module Test where

type MaybeCont a r = Maybe ((a -> r) -> r)
