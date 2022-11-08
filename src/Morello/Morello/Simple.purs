module Morello.Morello.Simple where

import Prelude (class Eq, class Show)

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Morello.Morello.Validated (ValidateE, ValidatedE)
import Morello.Morello.Core (PickE)

data ValidationError
  = FieldMissing String
  | FieldInvalid String

derive instance genericValidationError :: Generic ValidationError _

instance showValidationError :: Show ValidationError where
  show = genericShow

instance eqValidationError :: Eq ValidationError where
  eq = genericEq

type Validated r = ValidatedE ValidationError r
type Validate a b = ValidateE a ValidationError b
type Pick input a = PickE input ValidationError a
