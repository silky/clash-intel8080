{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE RankNTypes #-} -- Needed for `step` only
{-# LANGUAGE UndecidableInstances #-} -- Needed for the `TypeError` instance of `Meet` only

-- | A container for sequences where each step can have a pre- and a
-- postamble. The postamble and the preamble of two neighbouring steps
-- are collapsed.
module Hardware.Intel8080.Amble
       ( Step(..)
       , step
       , Amble(End), (>:>), (>++>)
       , stepsOf
       ) where

import Prelude ()
import Clash.Prelude
import Data.Singletons
import Data.Kind (Constraint)

class (SingKind a, SingKind b, SingI (MeetOf post pre)) => Meet (post :: Maybe b) (pre :: Maybe a) where
    type MeetOf post pre :: Maybe (Either a b)

type ConflictErr post pre =
    Text "Conflict between postamble " :<>: ShowType post :<>:
    Text " and next preamble " :<>: ShowType pre

instance (SingKind a, SingKind b) => Meet (Nothing :: Maybe b) (Nothing :: Maybe a) where
    type MeetOf Nothing Nothing = Nothing

instance (SingKind a, SingKind b, SingI post) => Meet (Just post :: Maybe b) (Nothing :: Maybe a) where
    type MeetOf (Just post) Nothing = Just (Right post)

instance (SingKind a, SingKind b, SingI pre) => Meet (Nothing :: Maybe b) (Just pre :: Maybe a) where
    type MeetOf Nothing (Just pre) = Just (Left pre)

instance (TypeError (ConflictErr post pre), SingKind a, SingKind b) => Meet (Just post :: Maybe b) (Just pre :: Maybe a) where
    type MeetOf (Just post) (Just pre) = Nothing

meetOf :: forall a b (post :: Maybe b) (pre :: Maybe a). (Meet post pre) => Sing post -> Sing pre -> Demote (Maybe (Either a b))
meetOf _ _ = demote @(MeetOf post pre)

data Step (pre :: Maybe a) (post :: Maybe b) t where
    Step :: Sing pre -> t -> Sing post -> Step pre post t
deriving instance Functor (Step pre post)

step :: forall pre. (SingI pre) => forall t. t -> forall post. (SingI post) => Step pre post t
step x = Step sing x sing

data Ends a b
    = Empty
    | NonEmpty (Maybe a) (Maybe b)

data Amble (n :: Nat) (ends :: Ends a b) t where
    End :: Amble 0 Empty t
    More
        :: forall (a0 :: Maybe a) (bn :: Maybe b) n t. ()
        => Sing a0
        -> Vec n (t, Demote (Maybe (Either a b)))
        -> t
        -> Sing bn
        -> Amble (1 + n) (NonEmpty a0 bn) t
deriving instance Functor (Amble n ends)

type family CanCons (b1 :: Maybe b) (ends :: Ends a b) :: Constraint where
    CanCons b1 Empty = ()
    CanCons b1 (NonEmpty a1 bn) = Meet b1 a1

type family Cons (a0 :: Maybe a) (b1 :: Maybe b) (ends :: Ends a b) where
    Cons a0 b1 Empty = NonEmpty a0 b1
    Cons a0 b1 (NonEmpty a1 bn) = NonEmpty a0 bn

cons
    :: forall a0 b1 n ends t. (CanCons b1 ends)
    => Step a0 b1 t
    -> Amble n ends t
    -> Amble (1 + n) (Cons a0 b1 ends) t
cons (Step a0 x b1) End = More a0 Nil x b1
cons (Step a0 x b1) (More a1 xs xn bn) = More a0 ((x, meetOf b1 a1) :> xs) xn bn

infixr 5 >:>
(>:>) = cons

type family CanAppend (ends1 :: Ends a b) (ends2 :: Ends a b) :: Constraint where
    CanAppend (NonEmpty a1 bn) ends2 = CanCons bn ends2
    CanAppend ends1 ends2 = ()

type family Append (ends1 :: Ends a b) (ends2 :: Ends a b) where
    Append Empty ends2 = ends2
    Append ends1 Empty = ends1
    Append (NonEmpty a0 bn) (NonEmpty an bm) = NonEmpty a0 bm

append :: (CanAppend ends1 ends2) => Amble n ends1 t -> Amble m ends2 t -> Amble (n + m) (Append ends1 ends2) t
append End ys = ys
append (More a0 xs xn bn) End = More a0 xs xn bn
append (More a0 xs xn bn) (More an ys ym bm) = More a0 (xs ++ singleton (xn, meetOf bn an) ++ ys) ym bm

infixr 5 >++>
(>++>) = append

stepsOf
    :: forall a b (ends :: Ends a b) n t. (SingKind a, SingKind b)
    => Amble n ends t
    -> (Maybe (Demote a), Vec n (t, Maybe (Demote (Either a b))))
stepsOf End = (Nothing, Nil)
stepsOf (More a0 xs xn bn) = (fromSing a0, xs :< (xn, Right <$> fromSing bn))
