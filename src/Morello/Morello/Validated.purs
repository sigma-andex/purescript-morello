module Morello.Morello.Validated where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as V

type ValidatedE err r
  = V (NonEmptyArray err) r

invalid :: forall err r. err -> V (NonEmptyArray err) r
invalid = NonEmpty.singleton >>> V.invalid

valid :: forall err r. r -> V (NonEmptyArray err) r
valid = pure

newtype ValidatorE input err a
  = ValidatorE (input -> ValidatedE err a)

applyValidator :: forall input err a. input -> ValidatorE input err a -> ValidatedE err a
applyValidator input (ValidatorE v) = v input

type ValidateE a err b
  = a -> ValidatedE err b

type ValidateE' a err = ValidateE a err a 
