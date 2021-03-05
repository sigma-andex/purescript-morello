module Morello.Morello.Core where

import Control.Semigroupoid (compose)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Lens (AGetter', view)
import Data.Profunctor.Strong ((&&&))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Validation.Semigroup (V)
import Heterogeneous.Folding (class FoldlRecord)
import Morello.Morello.Record (HMapKRec, SequenceRec, hmapKRec, sequenceRec)
import Morello.Morello.Validated (ValidatedE, ValidatorE(..), ValidateE, applyValidator, valid)
import Prelude (type (~>), const, identity, (<#>), (<$>), (<*>), (>>>))
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Record (union)
import Record.Builder (Builder)
import Type.Prelude (Proxy(..))

branch :: forall input err. input -> Tuple input (ValidatedE err {})
branch = identity &&& const (valid {})

infixr 8 branch as 🌱

dual ::
  forall input err from to' to.
  Union from to' to ⇒
  (input -> ValidatedE err { | to' }) ->
  Tuple input (ValidatedE err { | from }) ->
  Tuple input (ValidatedE err { | to })
dual f = fst &&& transform f
  where
  transform :: (input -> ValidatedE err { | to' }) -> Tuple input (ValidatedE err { | from }) -> (ValidatedE err { | to })
  transform f' tuple = Tuple <$> snd tuple <*> (f' (fst tuple)) <#> uncurry union

applyTemplate ::
  forall input err rin rinRL rthru rthruRL rout routRL.
  RowToList rin rinRL =>
  RowToList rthru rthruRL =>
  RowToList rout routRL =>
  FoldlRecord
    (HMapKRec (ValidatorE input err) (V (NonEmptyArray err)))
    (Builder (Record ()) (Record ()))
    rinRL
    rin
    (Builder (Record ()) (Record rthru)) =>
  FoldlRecord
    (SequenceRec (V (NonEmptyArray err)))
    (V (NonEmptyArray err) (Builder (Record ()) (Record ())))
    rthruRL
    rthru
    (V (NonEmptyArray err) (Builder (Record ()) (Record rout))) =>
  (ValidatorE input err ~> ValidatedE err) -> { | rin } -> ValidatedE err { | rout }
applyTemplate nt = hmapKRec nt >>> sequenceRec

cherry ::
  forall input err from to rin rinRL rthru rthruRL rout routRL.
  RowToList rin rinRL =>
  RowToList rthru rthruRL =>
  RowToList rout routRL =>
  FoldlRecord
    (HMapKRec (ValidatorE input err) (V (NonEmptyArray err)))
    (Builder (Record ()) (Record ()))
    rinRL
    rin
    (Builder (Record ()) (Record rthru)) =>
  FoldlRecord
    (SequenceRec (V (NonEmptyArray err)))
    (V (NonEmptyArray err) (Builder (Record ()) (Record ())))
    rthruRL
    rthru
    (V (NonEmptyArray err) (Builder (Record ()) (Record rout))) =>
  Union from rout to ⇒
  { | rin } ->
  Tuple input (ValidatedE err { | from }) ->
  Tuple input (ValidatedE err { | to })
cherry rin = dual f
  where
  f :: input -> ValidatedE err { | rout }
  f input = applyTemplate (applyValidator input) rin

infixr 8 cherry as 🍒

blossom :: forall input err output. Tuple input (ValidatedE err output) -> ValidatedE err output
blossom = snd

infixr 8 blossom as 🌸

infixr 9 compose as |>

pick :: forall s a err b. AGetter' s a -> ValidateE a err b -> ValidatorE s err b
pick lens validate = ValidatorE (view lens >>> validate)

pick' :: forall s a err b. Proxy s -> AGetter' s a -> ValidateE a err b -> ValidatorE s err b
pick' _ lens validate = pick lens validate

core :: forall f s a err b. Traversable f => AGetter' s (f a) -> ValidateE a err b -> ValidatorE s err (f b)
core lens validate = ValidatorE (view lens >>> traverse validate)

type Key r = Proxy r

key :: forall r. Key r
key = Proxy

type Typ r = Proxy r 

typ :: forall r. Proxy r
typ = Proxy