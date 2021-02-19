module Morello.Morello.Core where

import Control.Semigroupoid (compose)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Validation.Semigroup (V)
import Debug.Trace (spy)
import Heterogeneous.Folding (class FoldlRecord, class HFoldlWithIndex)
import Morello.Morello.Record (MappingPropOfK, SequencePropOf, mappingPropsOfK, sequencePropsOf)
import Morello.Validated (Validated, ValidationError, Validator, applyValidator, valid)
import Prelude (type (~>), const, identity, (#), ($), (<#>), (<$>), (<*>))
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


-- applyTemplate ::
--   ∀ input rin rinRL rout routRL.
--   HFoldlWithIndex (MappingPropOfK (Validator input) (V (NonEmptyArray ValidationError))) (Builder {} {}) { | rin }  (Builder {} { | rout } ) ⇒
--   RowToList rin rinRL =>
--   RowToList rout routRL =>
--     HFoldlWithIndex (SequencePropOf (V (NonEmptyArray ValidationError))) (V (NonEmptyArray ValidationError) (Builder {} {})) { | rin } (V (NonEmptyArray ValidationError) (Builder {} { | rout })) =>
--   { | rin } →
--   input ->
--   Validated { | rout }
applyTemplate :: forall input rin rinRL rout routRL. 
    RowToList rin rinRL =>
    RowToList rout routRL => 
    FoldlRecord (MappingPropOfK (Validator input) (V (NonEmptyArray ValidationError)))
                                      (Builder (Record ()) (Record ()))
                                      rinRL
                                      rin
                                      (Builder (Record ()) (Record rout )) =>
    FoldlRecord (SequencePropOf (V (NonEmptyArray ValidationError)))
                                      (V (NonEmptyArray ValidationError) (Builder (Record ()) (Record ())))
                                      routRL
                                      rout
                                      (V (NonEmptyArray ValidationError) (Builder (Record ()) (Record rout))) =>                            
    { | rin } -> input -> Validated { | rout }
applyTemplate rin input = mappingPropsOfK nt ( spy "before mapping" rin) # spy "after mapping" # sequencePropsOf
    where
      nt :: Validator input ~> Validated
      nt = (spy "validator" $ applyValidator input)

cherry ::
  forall input from to rin rinRL rout routRL .
  --HFoldlWithIndex (MappingPropOfK (Validator input) (V (NonEmptyArray ValidationError))) (Builder {} {}) { | rin }  (Builder {} { | rout } ) ⇒
  RowToList rin rinRL =>
  RowToList rout routRL =>
  FoldlRecord
    (MappingPropOfK (Validator input) (V (NonEmptyArray ValidationError)))
    (Builder {} {})
    rinRL
    rin
    (Builder {} { | rout } ) =>
  FoldlRecord
    (SequencePropOf (V (NonEmptyArray ValidationError)))
    (V (NonEmptyArray ValidationError) (Builder {} {}))
    routRL
    rout
    (V (NonEmptyArray ValidationError) (Builder {} { | rout } )) =>
  Union from rout to ⇒
  { | rin } ->
  Tuple input (Validated { | from }) ->
  Tuple input (Validated { | to })
cherry rin = dual f
    where
        f :: input -> Validated { | rout }
        f = applyTemplate rin

infixr 8 cherry as 🍒


blossom :: forall input output. Tuple input (Validated output) -> Validated output
blossom = snd

infixr 8 blossom as 🌸

infixr 9 compose as |>