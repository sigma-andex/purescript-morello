module Main where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Morello.Morello (Validate, Validated, ValidationError(..), blossom, branch, cherry, invalid, key, pick, valid, (|>), (🌱), (🌸), (🍒))
import Morello.Morello.Validated (Validator)

type Profession
  = { title :: String
    , salary :: Number
    }

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
  = { title :: Title, salary :: Salary }

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

type JobData
  = { title :: Title, salary :: Salary, jobType :: JobType }

type PersonOutput2
  = { jobData :: JobData }

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

professionL = prop (key :: _ "profession")

titleL = prop (key :: _ "title")

salaryL = prop (key :: _ "salary")

validateTitle :: Validate String Title
validateTitle "Software Engineer" = invalid (FieldInvalid "Software Engineering is not a serious profession")

validateTitle s = valid (Title s)

validateSalary :: Validate Number Salary
validateSalary n
  | n > 150000.0 = valid (Salary n)

validateSalary n = invalid (FieldInvalid "Salary is too low")

--pick = pickVP (Proxy :: Proxy PersonInput)
convert :: PersonInput -> Validated PersonOutput2
convert =
  branch
    >>> cherry
        { jobData:
            pick (professionL)
              ( branch
                  >>> cherry
                      { title: pick (titleL) validateTitle :: Validator Profession Title
                      , salary: pick (salaryL) validateSalary :: Validator Profession Salary
                      , jobType: Worker
                      }
                  >>> blossom
              ) ::
              Validator PersonInput JobData
        }
    >>> blossom

convert2 :: PersonInput -> Validated PersonOutput
convert2 =
  (🌱)
    >>> (🍒) { title: pick (professionL |> titleL) validateTitle :: Validator PersonInput Title }
    >>> (🍒) { salary: pick (professionL |> salaryL) validateSalary :: Validator PersonInput Salary }
    >>> (🌸)

convert3 :: PersonInput -> Validated PersonOutput2
convert3 =
  branch
    >>> cherry
        { jobData:
            { title: pick (professionL |> titleL) validateTitle :: Validator PersonInput Title
            , salary: pick (professionL |> salaryL) validateSalary :: Validator PersonInput Salary
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
