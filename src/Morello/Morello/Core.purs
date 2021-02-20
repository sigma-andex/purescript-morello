module Morello.Morello.Core where

import Control.Semigroupoid (compose)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Lens (AGetter', Lens', Iso', iso, lens', view)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong ((&&&))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Validation.Semigroup (V(..))
import Heterogeneous.Folding (class FoldlRecord)
import Morello.Morello.Record (MappingPropOfK, SequencePropOf, mappingPropsOfK, sequencePropsOf)
import Morello.Morello.Validated (Validated, ValidationError, Validator(..), Validate, applyValidator, valid)
import Prelude (type (~>), const, identity, (<#>), (<$>), (<*>), (>>>))
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Record (union)
import Record.Builder (Builder)
import Type.Prelude (Proxy)

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

asIs :: forall p t2 t3. Profunctor p => p t2 t3 -> p t2 t3
asIs = iso identity identity

as :: forall s a. Newtype s a => (a -> s) -> Iso' a s 
as _ = iso wrap unwrap

as' :: forall s a. Newtype s a => Iso' a s 
as' = iso wrap unwrap

validateL :: forall a. Validate a -> Lens' a (Validated a)
validateL validated = 
  lens' \field ->
    Tuple
      (validated field) 
      (\b -> case b of
        V (Left err) -> field
        V (Right v) -> v 
      )

validateOverL :: forall n a. Newtype n a => (a -> n) -> Validate a -> Lens' a (Validated n)
validateOverL _ validated = 
    lens' \field ->
        Tuple
        (validated field <#> wrap) 
        (\b -> case b of
            V (Left err) -> field
            V (Right v) -> unwrap v 
        )

infixr 9 compose as |>

pick :: forall s a. AGetter' s (Validated a) -> Validator s a
pick lens = Validator (view lens)

pickP :: forall s a. Proxy s -> AGetter' s (Validated a) -> Validator s a
pickP _ lens = Validator (view lens)

type Key r = SProxy r

key :: forall r. Key r
key = SProxy