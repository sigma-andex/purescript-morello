module Morello.Morello.Record where

import Prelude
import Data.Symbol (class IsSymbol, SProxy)
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Record.Builder as Builder

-- Helper for type inference
data SequencePropOf (f :: Type -> Type)
  = SequencePropOf

-- Matches if the type of the current field in the record is f a and therefore needs to be sequenced. 
instance sequencePropOf_1 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym a rb rc
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    (f a)
    (f (Builder { | ra } { | rc })) where
  foldingWithIndex _ prop rin a = (>>>) <$> rin <*> (Builder.insert prop <$> a)

-- Matches if the type of the current field in the record is another record and therefore needs to be recursed.
else instance sequencePropOf_2 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , RowToList x xRL
  , Row.Cons sym { | y } rb rc
  , FoldlRecord
      (SequencePropOf f)
      (f (Builder (Record ()) (Record ())))
      xRL
      x
      (f (Builder (Record ()) (Record y)))
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    { | x }
    (f (Builder { | ra } { | rc })) where
  foldingWithIndex _ prop rin x = (>>>) <$> rin <*> (fx <#> Builder.insert prop)
    where
    fx = sequencePropsOf x

-- Matches if the type of the current field in the record is any other type independent of sequencing.
else instance sequencePropOf_3 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (SequencePropOf f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    x
    (f (Builder { | ra } { | rc })) where
  foldingWithIndex _ prop rin x = (_ >>> Builder.insert prop x) <$> rin

-- | Recursively sequence a record. E.g.
-- | sequencePropsOf { a : { b : { c : { d: Just 10, e : Just "hello" }, f : Just true } == 
-- |  Just { a : { b : { c : { d: 10, e : "hello" }, f : true }
sequencePropsOf ::
  forall f rin rout.
  Applicative f =>
  HFoldlWithIndex (SequencePropOf f) (f (Builder {} {})) { | rin } (f (Builder {} { | rout })) =>
  { | rin } ->
  f { | rout }
sequencePropsOf =
  map (flip Builder.build {})
    <<< hfoldlWithIndex (SequencePropOf :: SequencePropOf f) (pure identity :: f (Builder {} {}))

-- Helper for type inference
data MappingPropOf a b
  = MappingPropOf (a -> b)

-- Matches if the type of the current field in the record is a and therefore needs to be mapped. 
instance mappingPropOf_1 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym b rb rc
  ) =>
  FoldingWithIndex
    (MappingPropOf a b)
    (SProxy sym)
    (Builder { | ra } { | rb })
    a
    (Builder { | ra } { | rc }) where
  foldingWithIndex (MappingPropOf f) prop rin a = (rin >>> Builder.insert prop (f a))

-- Matches if the type of the current field in the record is another record and therefore needs to be recursed.
else instance mappingPropOf_2 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , RowToList x xRL
  , Row.Cons sym { | y } rb rc
  , FoldlRecord
      (MappingPropOf a b)
      (Builder (Record ()) (Record ()))
      xRL
      x
      (Builder (Record ()) (Record y))
  ) =>
  FoldingWithIndex
    (MappingPropOf a b)
    (SProxy sym)
    (Builder { | ra } { | rb })
    { | x }
    (Builder { | ra } { | rc }) where
  foldingWithIndex (MappingPropOf f) prop rin x = (rin >>> Builder.insert prop fx)
    where
    fx = mappingPropsOf f x

-- Matches if the type of the current field in the record is any other type independent of mapping.
else instance mappingPropOf_3 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (MappingPropOf a b)
    (SProxy sym)
    (Builder { | ra } { | rb })
    x
    (Builder { | ra } { | rc }) where
  foldingWithIndex _ prop rin x = (rin >>> Builder.insert prop x)

-- | Recursively maps a record using a function f. E.g.
-- | let 
-- |   f :: Int -> String 
-- |   f i = show (i + 1)
-- | mappingPropsOf  f { a : { b : 10, c : { d: 20, e : Just "hello" }}, f : 30 } == 
-- |  { a : { b : "11", c : { d: "21", e : Just "hello" }, f : "31" }
mappingPropsOf ::
  forall a b rin rout.
  HFoldlWithIndex (MappingPropOf a b) (Builder {} {}) { | rin } (Builder {} { | rout }) =>
  (a -> b) ->
  { | rin } ->
  { | rout }
mappingPropsOf f =
  (flip Builder.build {})
    <<< hfoldlWithIndex (MappingPropOf f :: MappingPropOf a b) (identity :: Builder {} {})

-- Helper for type inference
data MappingPropOfK f g
  = MappingPropOfK (f ~> g)

-- Matches if the type of the current field in the record is f a and therefore needs to be naturally transformed. 
instance mappingPropOfK_1 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym (g a) rb rc
  ) =>
  FoldingWithIndex
    (MappingPropOfK f g)
    (SProxy sym)
    (Builder { | ra } { | rb })
    (f a)
    (Builder { | ra } { | rc }) where
  foldingWithIndex (MappingPropOfK nt) prop rin fa = (rin >>> Builder.insert prop (nt fa))

-- Matches if the type of the current field in the record is another record and therefore needs to be recursed.
else instance mappingPropOfK_2 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , RowToList x xRL
  , Row.Cons sym { | y } rb rc
  , FoldlRecord
      (MappingPropOfK f g)
      (Builder (Record ()) (Record ()))
      xRL
      x
      (Builder (Record ()) (Record y))
  ) =>
  FoldingWithIndex
    (MappingPropOfK f g)
    (SProxy sym)
    (Builder { | ra } { | rb })
    { | x }
    (Builder { | ra } { | rc }) where
  foldingWithIndex (MappingPropOfK nt) prop rin x = (rin >>> Builder.insert prop fx)
    where
    fx = mappingPropsOfK nt x

-- Matches if the type of the current field in the record is any other type independent of the natural transformation.
else instance mappingPropOfK_3 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (MappingPropOfK f g)
    (SProxy sym)
    (Builder { | ra } { | rb })
    x
    (Builder { | ra } { | rc }) where
  foldingWithIndex _ prop rin x = (rin >>> Builder.insert prop x)

-- | Recursively mapK a record using a natural transformation. E.g.
-- | let 
-- |   nt :: Either String ~> Maybe
-- |   nt = hush
-- | mappingPropsOfK { a : { b : { c : { d: Right 10, e : Left "hello" }, f : Right true } == 
-- |  Just { a : { b : { c : { d: Just 10, e : Nothing }, f : Just true }
mappingPropsOfK ::
  forall f g rin rout.
  HFoldlWithIndex (MappingPropOfK f g) (Builder {} {}) { | rin } (Builder {} { | rout }) =>
  (f ~> g) ->
  { | rin } ->
  { | rout }
mappingPropsOfK nt =
  (flip Builder.build {})
    <<< hfoldlWithIndex (MappingPropOfK nt :: MappingPropOfK f g) (identity :: Builder {} {})
