module Morello.Morello.Core where

import Control.Semigroupoid (compose)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Lens (AGetter', Forget, view)
import Data.Lens.Barlow (class Barlow, barlow)
import Data.Lens.Barlow.Construction (class ConstructBarlow)
import Data.Lens.Barlow.Parser (class ParseSymbol)
import Data.Lens.Barlow.Types (TList)
import Data.Profunctor.Strong ((&&&))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Validation.Semigroup (V)
import Heterogeneous.Extrablatt.Rec (HMapKRec, HSequenceRec, hmapKRec, hsequenceRec)
import Heterogeneous.Folding (class FoldlRecord)
import Morello.Morello.Validated (ValidatedE, ValidateE, valid)
import Prelude (type (~>), const, identity, (<#>), (<$>), (<*>), (>>>))
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Record (union)
import Record.Builder (Builder)
import Type.Prelude (Proxy(..))

newtype PickE input err a
  = PickE (ValidateE input err a)

applyPick :: forall input err a. input -> PickE input err a -> ValidatedE err a
applyPick input (PickE v) = v input

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
    (HMapKRec (PickE input err) (V (NonEmptyArray err)))
    (Builder (Record ()) (Record ()))
    rinRL
    rin
    (Builder (Record ()) (Record rthru)) =>
  FoldlRecord
    (HSequenceRec (V (NonEmptyArray err)))
    (V (NonEmptyArray err) (Builder (Record ()) (Record ())))
    rthruRL
    rthru
    (V (NonEmptyArray err) (Builder (Record ()) (Record rout))) =>
  (PickE input err ~> ValidatedE err) -> { | rin } -> ValidatedE err { | rout }
applyTemplate nt = hmapKRec nt >>> hsequenceRec

cherry ::
  forall input err from to rin rinRL rthru rthruRL rout routRL.
  RowToList rin rinRL =>
  RowToList rthru rthruRL =>
  RowToList rout routRL =>
  FoldlRecord
    (HMapKRec (PickE input err) (V (NonEmptyArray err)))
    (Builder (Record ()) (Record ()))
    rinRL
    rin
    (Builder (Record ()) (Record rthru)) =>
  FoldlRecord
    (HSequenceRec (V (NonEmptyArray err)))
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
  f input = applyTemplate (applyPick input) rin

infixr 8 cherry as 🍒

blossom :: forall input err output. Tuple input (ValidatedE err output) -> ValidatedE err output
blossom = snd

infixr 8 blossom as 🌸

pick' :: forall s a err b. AGetter' s a -> ValidateE a err b -> PickE s err b
pick' lens validate = PickE (view lens >>> validate)

--pick' :: forall err output s sym tlist input. ParseSymbol sym tlist => ConstructBarlow tlist (Forget input) { | s } input => Proxy sym -> ValidateE input err output -> PickE { | s } err output
pick ∷ ∀ (s31 ∷ Type) (err33 ∷ Type) (b34 ∷ Type) (b36 ∷ Type) (string39 ∷ Symbol) (t43 ∷ TList). ParseSymbol string39 t43 ⇒ ConstructBarlow t43 (Forget b36) s31 s31 b36 b36 ⇒ Proxy string39 → (b36 → V (NonEmptyArray err33) b34) → PickE s31 err33 b34
pick proxy = pick' (barlow proxy)

core' :: forall f s a err b. Traversable f => AGetter' s (f a) -> ValidateE a err b -> PickE s err (f b)
core' lens validate = PickE (view lens >>> traverse validate)

--core' :: forall f input err output sym tlist s. Traversable f => ParseSymbol sym tlist => ConstructBarlow tlist (Forget (f input)) { | s } (f input) => Proxy sym -> ValidateE input err output -> PickE { | s } err (f output)
core ∷ ∀ (f105 ∷ Type -> Type) (s106 ∷ Type) (a107 ∷ Type) (err108 ∷ Type) (b109 ∷ Type) (string114 ∷ Symbol) (t118 ∷ TList). Traversable f105 ⇒ ParseSymbol string114 t118 ⇒ ConstructBarlow t118 (Forget (f105 a107)) s106 s106 (f105 a107) (f105 a107) ⇒ Proxy string114 → (a107 → V (NonEmptyArray err108) b109) → PickE s106 err108 (f105 b109)
core proxy = core' (barlow proxy)

infixr 9 compose as |>
