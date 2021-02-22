module Morello.Morello.CompositionSpec where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Morello.Morello (Validate, Validated, ValidationError(..), blossom, branch, cherry, invalid, key, pick, valid, (|>), (🌱), (🌸), (🍒))
import Morello.Morello.Core (core)
import Morello.Morello.TestUtil (invalids)
import Morello.Morello.Validated (Validator)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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

derive instance titleEq :: Eq Title

instance titleShow :: Show Title where
  show = genericShow

newtype Salary
  = Salary Number

derive instance salaryNT :: Newtype Salary _

derive instance salaryGen :: Generic Salary _

derive instance salaryEq :: Eq Salary

instance salaryShow :: Show Salary where
  show = genericShow

data JobType
  = Worker
  | Manager

derive instance jobTypeGen :: Generic JobType _

derive instance jobTypeEq :: Eq JobType

instance jobTypeShow :: Show JobType where
  show = genericShow

newtype Zip
  = Zip Int

derive instance zipNT :: Newtype Zip _

derive instance zipGen :: Generic Zip _

derive instance zipEq :: Eq Zip

instance zipShow :: Show Zip where
  show = genericShow

type Address
  = { zip :: Zip }

type JobData
  = { title :: Title, salary :: Salary, jobType :: JobType }

type Person
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

convert :: InputPerson -> Validated Person
convert =
  branch
    >>> cherry
        { jobData:
            { title: pick (professionL |> titleL) validateTitle :: Validator InputPerson Title
            , salary: pick (professionL |> salaryL) validateSalary :: Validator InputPerson Salary
            , jobType: Worker
            }
        , addresses:
            core (personL |> addressesL)
              ( branch
                  >>> cherry
                      { zip: pick (zipL) validateZip :: Validator InputAddress Zip
                      }
                  >>> blossom
              ) ::
              Validator InputPerson (Array Address)
        }
    >>> blossom

convert2 :: InputPerson -> Validated Person
convert2 =
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
            core (personL |> addressesL)
              ( branch
                  >>> cherry
                      { zip: pick (zipL) validateZip :: Validator InputAddress Zip
                      }
                  >>> blossom
              ) ::
              Validator InputPerson (Array Address)
        }
    >>> blossom

convert3 :: InputPerson -> Validated Person
convert3 =
  (🌱)
    >>> (🍒)
        { jobData:
            { title: pick (professionL |> titleL) validateTitle :: Validator InputPerson Title
            , salary: pick (professionL |> salaryL) validateSalary :: Validator InputPerson Salary
            , jobType: Worker
            }
        }
    >>> (🍒)
        { addresses:
            core (personL |> addressesL)
              ( branch
                  >>> cherry
                      { zip: pick (zipL) validateZip :: Validator InputAddress Zip
                      }
                  >>> blossom
              ) ::
              Validator InputPerson (Array Address)
        }
    >>> (🌸)

expectedValid :: Validated Person
expectedValid =
  pure
    $ { jobData:
          { title: Title "Pilot"
          , salary: Salary 200000.0
          , jobType: Worker
          }
      , addresses:
          [ { zip: Zip 12300
            }
          , { zip: Zip 45600
            }
          ]
      }

expectedInvalid :: Maybe (Validated Person)
expectedInvalid =
  invalids
    $ [ FieldInvalid "Zip 123 out of range"
      , FieldInvalid "asdf is not a valid zip"
      , FieldInvalid "Salary of 120000.0 is too low"
      , FieldInvalid "Software Engineering is not a serious profession"
      ]

expectedInvalid2 :: Maybe (Validated Person)
expectedInvalid2 =
  invalids
    $ [ FieldInvalid "Salary of 120000.0 is too low"
      , FieldInvalid "Software Engineering is not a serious profession"
      , FieldInvalid "Zip 123 out of range"
      , FieldInvalid "asdf is not a valid zip"
      ]

expectedInvalid3 :: Maybe (Validated Person)
expectedInvalid3 =
  invalids
    $ [ FieldInvalid "Salary of 120000.0 is too low"
      , FieldInvalid "Software Engineering is not a serious profession"
      , FieldInvalid "Zip 123 out of range"
      , FieldInvalid "asdf is not a valid zip"
      ]

spec :: Spec Unit
spec =
  describe "Morello.Morello" do
    describe "cherry" do
      it "should convert a valid data structure" do
        let
          actual = convert validPerson
        actual `shouldEqual` expectedValid
        let
          actual2 = convert invalidPerson
        (Just actual2) `shouldEqual` expectedInvalid
      it "should support composition" do
        let
          actual = convert2 validPerson
        actual `shouldEqual` expectedValid
        let
          actual2 = convert2 invalidPerson
        (Just actual2) `shouldEqual` expectedInvalid2
      it "should support 🍒 unicode 🌸" do
        let
          actual = convert3 validPerson
        actual `shouldEqual` expectedValid
        let
          actual2 = convert3 invalidPerson
        (Just actual2) `shouldEqual` expectedInvalid3
