module Morello.Morello.Record where

import Prelude
import Data.Symbol (class IsSymbol, SProxy)
import Debug.Trace (spy)
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Record.Builder as Builder

data SequencePropOf (f :: Type -> Type)
  = SequencePropOf

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
  foldingWithIndex _ prop rin a = (>>>) <$> rin <*> (Builder.insert prop <$> a) # spy "s1"
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
  foldingWithIndex _ prop rin x = (>>>) <$> rin <*> (fx <#> Builder.insert prop) # spy "s2"
    where
    fx = sequencePropsOf x
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
  foldingWithIndex _ prop rin x = (_ >>> Builder.insert prop x) <$> rin # spy "s3"


sequencePropsOf ::
  forall f rin rout.
  Applicative f =>
  HFoldlWithIndex (SequencePropOf f) (f (Builder {} {})) { | rin } (f (Builder {} { | rout })) =>
  { | rin } ->
  f { | rout }
sequencePropsOf =
  map (flip Builder.build {})
    <<< hfoldlWithIndex (SequencePropOf :: SequencePropOf f) (pure identity :: f (Builder {} {}))

data MappingPropOf a b
  = MappingPropOf (a -> b)

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
  foldingWithIndex (MappingPropOf f) prop rin a = (rin >>> Builder.insert prop (f a)) # spy "m1"
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
  foldingWithIndex (MappingPropOf f) prop rin x = (rin >>> Builder.insert prop fx) # spy "m2"
    where
    fx = mappingPropsOf f x
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
  foldingWithIndex _ prop rin x = (rin >>> Builder.insert prop x) # spy "m3"

mappingPropsOf ::
  forall f a b rin rout.
  HFoldlWithIndex (MappingPropOf a b) (Builder {} {}) { | rin } (Builder {} { | rout }) =>
  (a -> b) ->
  { | rin } ->
  { | rout }
mappingPropsOf f =
  (flip Builder.build {})
    <<< hfoldlWithIndex (MappingPropOf f :: MappingPropOf a b) (identity :: Builder {} {})

data MappingPropOfK f g
  = MappingPropOfK (f ~> g)

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
  foldingWithIndex (MappingPropOfK nt) prop rin fa = (rin >>> Builder.insert prop (nt fa)) # spy "mk1"
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
  foldingWithIndex (MappingPropOfK nt) prop rin x = (rin >>> Builder.insert prop fx) # spy "mk2"
    where
    fx = mappingPropsOfK nt x
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
  foldingWithIndex _ prop rin x = (rin >>> Builder.insert prop x) # spy "mk3"

mappingPropsOfK ::
  forall f g a rin rout.
  HFoldlWithIndex (MappingPropOfK f g) (Builder {} {}) { | rin } (Builder {} { | rout }) =>
  (f ~> g) ->
  { | rin } ->
  { | rout }
mappingPropsOfK nt =
  (flip Builder.build {})
    <<< hfoldlWithIndex (MappingPropOfK nt :: MappingPropOfK f g) (identity :: Builder {} {})
