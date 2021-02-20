module Morello.Morello.Record where

import Prelude
import Data.Symbol (class IsSymbol, SProxy)
import Heterogeneous.Folding (class FoldingWithIndex, class FoldlRecord, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record.Builder (Builder)
import Record.Builder as Builder

-- Helper for type inference
data SequenceRec (f :: Type -> Type)
  = SequenceRec

-- Matches if the type of the current field in the record is f a and therefore needs to be sequenced. 
instance sequenceRec_1 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym a rb rc
  ) =>
  FoldingWithIndex
    (SequenceRec f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    (f a)
    (f (Builder { | ra } { | rc })) where
  foldingWithIndex _ prop rin a = (>>>) <$> rin <*> (Builder.insert prop <$> a)

-- Matches if the type of the current field in the record is another record and therefore needs to be recursed.
else instance sequenceRec_2 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , RowToList x xRL
  , Row.Cons sym { | y } rb rc
  , FoldlRecord
      (SequenceRec f)
      (f (Builder (Record ()) (Record ())))
      xRL
      x
      (f (Builder (Record ()) (Record y)))
  ) =>
  FoldingWithIndex
    (SequenceRec f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    { | x }
    (f (Builder { | ra } { | rc })) where
  foldingWithIndex _ prop rin x = (>>>) <$> rin <*> (fx <#> Builder.insert prop)
    where
    fx = sequenceRec x

-- Matches if the type of the current field in the record is any other type independent of sequencing.
else instance sequenceRec_3 ::
  ( Applicative f
  , IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (SequenceRec f)
    (SProxy sym)
    (f (Builder { | ra } { | rb }))
    x
    (f (Builder { | ra } { | rc })) where
  foldingWithIndex _ prop rin x = (_ >>> Builder.insert prop x) <$> rin

-- | Recursively sequence a record. E.g.
-- | sequenceRec { a : { b : { c : { d: Just 10, e : Just "hello" }, f : Just true } == 
-- |  Just { a : { b : { c : { d: 10, e : "hello" }, f : true }
sequenceRec ::
  forall f rin rout.
  Applicative f =>
  HFoldlWithIndex (SequenceRec f) (f (Builder {} {})) { | rin } (f (Builder {} { | rout })) =>
  { | rin } ->
  f { | rout }
sequenceRec =
  map (flip Builder.build {})
    <<< hfoldlWithIndex (SequenceRec :: SequenceRec f) (pure identity :: f (Builder {} {}))

-- Helper for type inference
data HMapRec a b
  = HMapRec (a -> b)

-- Matches if the type of the current field in the record is a and therefore needs to be mapped. 
instance hmapRec_1 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym b rb rc
  ) =>
  FoldingWithIndex
    (HMapRec a b)
    (SProxy sym)
    (Builder { | ra } { | rb })
    a
    (Builder { | ra } { | rc }) where
  foldingWithIndex (HMapRec f) prop rin a = (rin >>> Builder.insert prop (f a))

-- Matches if the type of the current field in the record is another record and therefore needs to be recursed.
else instance hmapRec_2 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , RowToList x xRL
  , Row.Cons sym { | y } rb rc
  , FoldlRecord
      (HMapRec a b)
      (Builder (Record ()) (Record ()))
      xRL
      x
      (Builder (Record ()) (Record y))
  ) =>
  FoldingWithIndex
    (HMapRec a b)
    (SProxy sym)
    (Builder { | ra } { | rb })
    { | x }
    (Builder { | ra } { | rc }) where
  foldingWithIndex (HMapRec f) prop rin x = (rin >>> Builder.insert prop fx)
    where
    fx = hmapRec f x

-- Matches if the type of the current field in the record is any other type independent of mapping.
else instance hmapRec_3 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (HMapRec a b)
    (SProxy sym)
    (Builder { | ra } { | rb })
    x
    (Builder { | ra } { | rc }) where
  foldingWithIndex _ prop rin x = (rin >>> Builder.insert prop x)

-- | Recursively maps a record using a function f. E.g.
-- | let 
-- |   f :: Int -> String 
-- |   f i = show (i + 1)
-- | hmapRec  f { a : { b : 10, c : { d: 20, e : Just "hello" }}, f : 30 } == 
-- |  { a : { b : "11", c : { d: "21", e : Just "hello" }, f : "31" }
hmapRec ::
  forall a b rin rout.
  HFoldlWithIndex (HMapRec a b) (Builder {} {}) { | rin } (Builder {} { | rout }) =>
  (a -> b) ->
  { | rin } ->
  { | rout }
hmapRec f =
  (flip Builder.build {})
    <<< hfoldlWithIndex (HMapRec f :: HMapRec a b) (identity :: Builder {} {})

-- Helper for type inference
data HMapKRec f g
  = HMapKRec (f ~> g)

-- Matches if the type of the current field in the record is f a and therefore needs to be naturally transformed. 
instance hmapRecK_1 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym (g a) rb rc
  ) =>
  FoldingWithIndex
    (HMapKRec f g)
    (SProxy sym)
    (Builder { | ra } { | rb })
    (f a)
    (Builder { | ra } { | rc }) where
  foldingWithIndex (HMapKRec nt) prop rin fa = (rin >>> Builder.insert prop (nt fa))

-- Matches if the type of the current field in the record is another record and therefore needs to be recursed.
else instance hmapRecK_2 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , RowToList x xRL
  , Row.Cons sym { | y } rb rc
  , FoldlRecord
      (HMapKRec f g)
      (Builder (Record ()) (Record ()))
      xRL
      x
      (Builder (Record ()) (Record y))
  ) =>
  FoldingWithIndex
    (HMapKRec f g)
    (SProxy sym)
    (Builder { | ra } { | rb })
    { | x }
    (Builder { | ra } { | rc }) where
  foldingWithIndex (HMapKRec nt) prop rin x = (rin >>> Builder.insert prop fx)
    where
    fx = hmapKRec nt x

-- Matches if the type of the current field in the record is any other type independent of the natural transformation.
else instance hmapRecK_3 ::
  ( IsSymbol sym
  , Row.Lacks sym rb
  , Row.Cons sym x rb rc
  ) =>
  FoldingWithIndex
    (HMapKRec f g)
    (SProxy sym)
    (Builder { | ra } { | rb })
    x
    (Builder { | ra } { | rc }) where
  foldingWithIndex _ prop rin x = (rin >>> Builder.insert prop x)

-- | Recursively mapK a record using a natural transformation. E.g.
-- | let 
-- |   nt :: Either String ~> Maybe
-- |   nt = hush
-- | hmapKRec { a : { b : { c : { d: Right 10, e : Left "hello" }, f : Right true } == 
-- |  Just { a : { b : { c : { d: Just 10, e : Nothing }, f : Just true }
hmapKRec ::
  forall f g rin rout.
  HFoldlWithIndex (HMapKRec f g) (Builder {} {}) { | rin } (Builder {} { | rout }) =>
  (f ~> g) ->
  { | rin } ->
  { | rout }
hmapKRec nt =
  (flip Builder.build {})
    <<< hfoldlWithIndex (HMapKRec nt :: HMapKRec f g) (identity :: Builder {} {})
