module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (traversed)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Morello.Morello.Core (blossom, branch, cherry, key, pickP, validateL, validateOverL, (|>), (🌱), (🌸), (🍒))
import Morello.Validated (Validate, Validated, ValidationError(..), invalid, valid)
import Type.Prelude (Proxy(..))

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


type PersonOutput
  = { title :: String, salary :: Number }


newtype Title
  = Title String

derive instance titleNT :: Newtype Title _

derive instance titleGen :: Generic Title _

instance titleShow :: Show Title where
  show = genericShow

newtype Salary
  = Salary Number

derive instance salaryNT :: Newtype Salary _

derive instance salaryGen :: Generic Salary _

instance salaryShow :: Show Salary where
  show = genericShow

data JobType
  = Worker
  | Manager

derive instance jobTypeGen :: Generic JobType _

instance jobTypeShow :: Show JobType where
  show = genericShow

type PersonOutput2
  = { details :: { title :: Title, salary :: Salary, jobType :: JobType } }

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

personL = prop (key :: _ "person")

addressesL = prop (key :: _ "addresses")

zipL = prop (key :: _ "zip")

professionL = prop (key :: _ "profession")

titleL = prop (key :: _ "title")

salaryL = prop (key :: _ "salary")

addresses = personL <<< addressesL <<< traversed



titleValidator :: Validate String
titleValidator "Software Engineer" = invalid (FieldInvalid "Software Engineering is not a serious profession")

titleValidator s = valid s

salaryValidator :: Validate Number
salaryValidator n
  | n > 150000.0 = valid n

salaryValidator n = invalid (FieldInvalid "Salary is too low")


pickV = pickP (Proxy :: Proxy PersonInput)



convert :: PersonInput -> Validated PersonOutput
convert =
  branch
    >>> cherry
        { title:
            pickV (professionL |> titleL |> validateL titleValidator)
        , salary:
            pickV (professionL |> salaryL |> validateL salaryValidator)
        }
    >>> blossom


convert2 :: PersonInput -> Validated PersonOutput
convert2 =
  (🌱)
    >>> (🍒) { title: pickV (professionL |> titleL |> validateL titleValidator) }
    >>> (🍒) { salary: pickV (professionL |> salaryL |> validateL salaryValidator) }
    >>> (🌸)



convert3 :: PersonInput -> Validated PersonOutput2
convert3 =
  branch
    >>> cherry
        { details:
            { title: pickV (professionL |> titleL |> validateOverL Title titleValidator) 
            , salary: pickV (professionL |> salaryL |> validateOverL Salary salaryValidator) 
            , jobType: Worker
            }
        }
    >>> blossom

main :: Effect Unit
main = do
  log "\n----example 1----\n"
  logShow $ convert invalidPerson
  logShow $ convert validPerson
  log "\n----example 2----\n"
  logShow $ convert2 invalidPerson
  logShow $ convert2 validPerson
  log "\n----example 3----\n"
  logShow $ convert3 invalidPerson
  logShow $ convert3 validPerson
