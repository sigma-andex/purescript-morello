module Main where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Morello.Morello (Validate, Validated, ValidationError(..), blossom, branch, cherry, invalid, key, pick, valid, (|>), (🌱), (🌸), (🍒))
import Morello.Morello.Core (unpit)
import Morello.Morello.Validated (Validator)

type InputProfession
  = { title :: String
    , salary :: Number
    }

type InputAddress
  = { zip :: String }

type InputPerson
  = { person ::
        { addresses ::
            Array InputAddress
        }
    , profession :: InputProfession
    }

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

newtype Zip
  = Zip Int

derive instance zipNT :: Newtype Zip _

derive instance zipGen :: Generic Zip _

instance zipShow :: Show Zip where
  show = genericShow

type Address
  = { zip :: Zip }

type JobData
  = { title :: Title, salary :: Salary, jobType :: JobType }

type PersonOutput
  = { jobData :: JobData, addresses :: Array Address }

invalidPerson :: InputPerson
invalidPerson =
  { person:
      { addresses:
          [ { zip: "123"
            }
          , { zip: "asdf"
            }
          ]
      }
  , profession:
      { title: "Software Engineer"
      , salary: 120000.0
      }
  }

validPerson :: InputPerson
validPerson =
  { person:
      { addresses:
          [ { zip: "12300"
            }
          , { zip: "45600"
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

vacationL = prop (key :: _ "customVacation")

validateTitle :: Validate String Title
validateTitle "Software Engineer" = invalid (FieldInvalid "Software Engineering is not a serious profession")

validateTitle s = valid (Title s)

validateSalary :: Validate Number Salary
validateSalary n
  | n > 150000.0 = valid (Salary n)

validateSalary n = invalid (FieldInvalid $ "Salary of " <> show n <> " is too low")

validateZip :: Validate String Zip
validateZip n = case fromString n of
  Just zip
    | 10000 <= zip && zip <= 99999 -> valid (Zip zip)
  Just zip -> invalid (FieldInvalid $ "Zip " <> n <> " out of range")
  Nothing -> invalid (FieldInvalid $ n <> " is not a valid zip")

convert :: InputPerson -> Validated PersonOutput
convert =
  branch
    >>> cherry
        { jobData:
            pick (professionL)
              ( branch
                  >>> cherry
                      { title: pick (titleL) validateTitle :: Validator InputProfession Title
                      , salary: pick (salaryL) validateSalary :: Validator InputProfession Salary
                      , jobType: Worker
                      }
                  >>> blossom
              ) ::
              Validator InputPerson JobData
        }
    >>> cherry
        { addresses:
            unpit (personL |> addressesL)
              ( branch
                  >>> cherry
                      { zip: pick (zipL) validateZip :: Validator InputAddress Zip
                      }
                  >>> blossom
              ) ::
              Validator InputPerson (Array Address)
        }
    >>> blossom

convert2 :: InputPerson -> Validated PersonOutput
convert2 =
  (🌱)
    >>> (🍒)
        { jobData:
            { title: pick (professionL |> titleL) validateTitle :: Validator InputPerson Title
            , salary: pick (professionL |> salaryL) validateSalary :: Validator InputPerson Salary
            , jobType: Worker
            }
        }
    >>> (🍒) { addresses: [] }
    >>> (🌸)

convert3 :: InputPerson -> Validated PersonOutput
convert3 =
  branch
    >>> cherry
        { jobData:
            { title: pick (professionL |> titleL) validateTitle :: Validator InputPerson Title
            , salary: pick (professionL |> salaryL) validateSalary :: Validator InputPerson Salary
            , jobType: Worker
            }
        , addresses:
            unpit (personL |> addressesL)
              ( branch
                  >>> cherry
                      { zip: pick (zipL) validateZip :: Validator InputAddress Zip
                      }
                  >>> blossom
              ) ::
              Validator InputPerson (Array Address)
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
