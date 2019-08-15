module Pile where

import Card

type Visible = Card
type Hidden = Card

data Pile = Pile Visible (Hidden, Hidden)

instance Show Pile where
    show (Pile v _) = show v ++ "🂠" ++ "🂠"
