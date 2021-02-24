module Morello.Morello.TestUtil where

import Prelude

import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Validation.Semigroup (V(..))
import Morello.Morello (ValidatedE)

invalids :: forall r err. Array err -> Maybe (ValidatedE err r)
invalids errs = 
    NonEmpty.fromArray errs <#> V <<< Left 
