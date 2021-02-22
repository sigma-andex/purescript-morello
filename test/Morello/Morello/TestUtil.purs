module Morello.Morello.TestUtil where

import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Validation.Semigroup (V(..))
import Morello.Morello (Validated, ValidationError)

invalids :: forall r. Array ValidationError -> Maybe (Validated r)
invalids errs = 
    NonEmpty.fromArray errs <#> V <<< Left 