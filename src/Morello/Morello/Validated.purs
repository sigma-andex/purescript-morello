module Morello.Morello.Validated where

import Prelude
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as V

data ValidationError
  = FieldMissing String
  | FieldInvalid String

derive instance genericValidationError :: Generic ValidationError _

instance showValidationError :: Show ValidationError where
  show = genericShow

type Validated r
  = V (NonEmptyArray ValidationError) r

invalid :: forall err r. err -> V (NonEmptyArray err) r
invalid = NonEmpty.singleton >>> V.invalid

valid :: forall err r. r -> V (NonEmptyArray err) r
valid = pure

newtype Validator input a
  = Validator (input -> Validated a)

applyValidator :: forall input a. input -> Validator input a -> Validated a
applyValidator input (Validator v) = v input

type Validate a
  = a -> Validated a
