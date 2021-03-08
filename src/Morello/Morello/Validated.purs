module Morello.Morello.Validated where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as V
import Data.Newtype (class Newtype, wrap)

type ValidatedE err r
  = V (NonEmptyArray err) r

type ValidateE a err b
  = a -> ValidatedE err b

type ValidateE' a err = ValidateE a err a 

newtype ValidatorE input err a
  = ValidatorE (input -> ValidatedE err a)

applyValidator :: forall input err a. input -> ValidatorE input err a -> ValidatedE err a
applyValidator input (ValidatorE v) = v input


invalid :: forall err r. err -> ValidatedE err r
invalid = NonEmpty.singleton >>> V.invalid

valid :: forall err r. r -> ValidatedE err r
valid = pure

as :: forall input err a. Newtype a input => (input -> a) -> ValidateE input err a
as _ = wrap >>> valid

asIs :: forall input err. ValidateE' input err
asIs = valid