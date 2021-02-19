module Main where

import Data.Lens (traversed)
import Data.Lens.Getter (view)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (log)
import Morello.Morello.Core (blossom, branch, cherry, (🌱), (🌸), (🍒))
import Morello.Validated (Validated, ValidationError(..), Validator(..), invalid, valid)
import Prelude (Unit, (<<<), (>), (>>>))

type PersonInput
  = { person ::
        { addresses ::
            Array
              { zip :: String
              }
        }
    , profession ::
        { title :: String
        , salary :: Number
        }
    }

invalidPerson :: PersonInput
invalidPerson =
  { person:
      { addresses:
          [ { zip: "123"
            }
          , { zip: "456"
            }
          ]
      }
  , profession:
      { title: "Software Engineer"
      , salary: 120000.0
      }
  }

validPerson :: PersonInput
validPerson =
  { person:
      { addresses:
          [ { zip: "123"
            }
          , { zip: "456"
            }
          ]
      }
  , profession:
      { title: "Pilot"
      , salary: 200000.0
      }
  }

personL = prop (SProxy :: SProxy "person")

addressesL = prop (SProxy :: SProxy "addresses")

zipL = prop (SProxy :: SProxy "zip")

professionL = prop (SProxy :: SProxy "profession")

titleL = prop (SProxy :: SProxy "title")

salaryL = prop (SProxy :: SProxy "salary")

addresses = personL <<< addressesL <<< traversed

titleValidator :: Validator PersonInput String
titleValidator = Validator (view (professionL <<< titleL) >>> f)
  where
    f :: String -> Validated String 
    f "Software Engineer" = invalid (FieldInvalid "Software Engineering is not a serious profession")
    f s = valid s 

salaryValidator :: Validator PersonInput Number
salaryValidator = Validator (view (professionL <<< salaryL) >>> f)
  where
    f :: Number -> Validated Number
    f n | n > 150000.0 = valid n
    f n = invalid (FieldInvalid "Salary is too low")


type PersonOutput
  = { title :: String, salary :: Number }

convert :: PersonInput -> Validated PersonOutput
convert =
  branch
    >>> cherry { title: titleValidator }
    >>> cherry { salary: salaryValidator }
    >>> blossom

convert2 :: PersonInput -> Validated PersonOutput
convert2 =
  (🌱)
    >>> (🍒) { title: titleValidator }
    >>> (🍒) { salary: salaryValidator }
    >>> (🌸)

convert3 :: PersonInput -> Validated PersonOutput
convert3 =
  branch
    >>> cherry { title: titleValidator, salary: salaryValidator }
    >>> blossom

main :: Effect Unit
main = do
  let
    _ = spy "convert" (convert invalidPerson)
    _ = spy "convert" (convert validPerson)

    _ = spy "convert2" (convert2 invalidPerson)
    _ = spy "convert2" (convert2 validPerson)

    _ = spy "convert3" (convert3 invalidPerson)
    _ = spy "convert3" (convert3 validPerson)
  log ""
