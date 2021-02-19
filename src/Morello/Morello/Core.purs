module Morello.Morello.Core where

import Control.Semigroupoid (compose)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Validation.Semigroup (V)
import Heterogeneous.Folding (class FoldlRecord)
import Morello.Morello.Record (MappingPropOfK, SequencePropOf, mappingPropsOfK, sequencePropsOf)
import Morello.Validated (Validated, ValidationError, Validator, applyValidator, valid)
import Prelude (type (~>), const, identity, (<#>), (<$>), (<*>), (>>>))
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Record (union)
import Record.Builder (Builder)

branch :: forall input. input -> Tuple input (Validated {})
branch = identity &&& const (valid {})

infixr 8 branch as 🌱

dual ::
  forall input from to' to.
  Union from to' to ⇒
  (input -> Validated { | to' }) ->
  Tuple input (Validated { | from }) ->
  Tuple input (Validated { | to })
dual f = fst &&& transform f
  where
  transform :: (input -> Validated { | to' }) -> Tuple input (Validated { | from }) -> (Validated { | to })
  transform f' tuple = Tuple <$> snd tuple <*> (f' (fst tuple)) <#> uncurry union

applyTemplate ::
  forall input rin rinRL rthru rthruRL rout routRL.
  RowToList rin rinRL =>
  RowToList rthru rthruRL =>
  RowToList rout routRL =>
  FoldlRecord
    (MappingPropOfK (Validator input) (V (NonEmptyArray ValidationError)))
    (Builder (Record ()) (Record ()))
    rinRL
    rin
    (Builder (Record ()) (Record rthru)) =>
  FoldlRecord
    (SequencePropOf (V (NonEmptyArray ValidationError)))
    (V (NonEmptyArray ValidationError) (Builder (Record ()) (Record ())))
    rthruRL
    rthru
    (V (NonEmptyArray ValidationError) (Builder (Record ()) (Record rout))) =>
  (Validator input ~> Validated) -> { | rin } -> Validated { | rout }
applyTemplate nt = mappingPropsOfK nt >>> sequencePropsOf

cherry ::
  forall input from to rin rinRL rthru rthruRL rout routRL.
  RowToList rin rinRL =>
  RowToList rthru rthruRL =>
  RowToList rout routRL =>
  FoldlRecord
    (MappingPropOfK (Validator input) (V (NonEmptyArray ValidationError)))
    (Builder (Record ()) (Record ()))
    rinRL
    rin
    (Builder (Record ()) (Record rthru)) =>
  FoldlRecord
    (SequencePropOf (V (NonEmptyArray ValidationError)))
    (V (NonEmptyArray ValidationError) (Builder (Record ()) (Record ())))
    rthruRL
    rthru
    (V (NonEmptyArray ValidationError) (Builder (Record ()) (Record rout))) =>
  Union from rout to ⇒
  { | rin } ->
  Tuple input (Validated { | from }) ->
  Tuple input (Validated { | to })
cherry rin = dual f
  where
  f :: input -> Validated { | rout }
  f input = applyTemplate (applyValidator input) rin

infixr 8 cherry as 🍒

blossom :: forall input output. Tuple input (Validated output) -> Validated output
blossom = snd

infixr 8 blossom as 🌸

infixr 9 compose as |>
