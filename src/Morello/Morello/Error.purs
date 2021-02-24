module Morello.Morello.Error where

import Prelude (class Eq, class Show)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Morello.Morello.Validated (ValidateE, ValidatedE, ValidatorE) 

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
type Validator input a = ValidatorE input ValidationError a