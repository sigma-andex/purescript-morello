module Morello.Morello.MinimalSpec where

import Morello.Morello (Validate, Validated, ValidationError(..), blossom, branch, cherry, invalid, key, pick', valid, (|>))
import Prelude (class Eq, class Show, Unit, discard, (>), (>>>))

import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Validation.Semigroup (V(..))
import Morello.Morello.Validated (Validator)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- define your (weakly typed) input model
type PersonInput
  = { profession ::
        { title :: String
        , salary :: Number
        }
    }

-- define your (strongly typed) output model
newtype Title = Title String
derive instance titleNT :: Newtype Title _

newtype Salary = Salary Number
derive instance salaryNT :: Newtype Salary _

data JobType = Worker | Manager

type PersonOutput
  = { details :: { title :: Title, salary :: Salary, jobType :: JobType } }

-- define lenses for accessing the input object
professionL = prop (key :: _ "profession")

titleL = prop (key :: _ "title")

salaryL = prop (key :: _ "salary")

-- write your (business logic) validation
validateTitle :: Validate String Title
validateTitle "Software Engineer" = invalid (FieldInvalid "Software Engineering is not a serious profession")
validateTitle s = valid (Title s)

validateSalary :: Validate Number Salary
validateSalary n 
    | n > 50000.0 = valid (Salary n)
validateSalary n = invalid (FieldInvalid "Salary is too damn low")

-- define your convert function
convert :: PersonInput -> Validated PersonOutput
convert =
  branch -- start with a branch
    >>> cherry { -- then start cherry picking
            details : { -- by defining how your output format should look like
                title: 
                    -- then pick data from your input and validate them 
                    pick' (professionL |> titleL ) validateTitle :: Validator PersonInput Title
              , salary:
                    pick' (professionL |> salaryL ) validateSalary :: Validator PersonInput Salary
              -- you can also set constant data
              , jobType : Worker
            }
        }
    >>> blossom

-- convert your data
invalidPerson :: PersonInput
invalidPerson =
  { profession:
      { title: "Software Engineer"
      , salary: 30000.0
      }
  }

validPerson :: PersonInput
validPerson =
  { profession:
      { title: "Pilot"
      , salary: 200000.0
      }
  }

first :: Validated PersonOutput
first = convert invalidPerson
-- invalid ((NonEmptyArray [(FieldInvalid "Salary is too damn low"),(FieldInvalid "Software Engineering is not a serious profession")]))

second :: Validated PersonOutput
second = convert validPerson
-- pure ({ details: { jobType: Worker, salary: (Salary 200000.0), title: (Title "Pilot") } })

instance salaryShow :: Show Salary where
  show = genericShow
derive instance salaryEq :: Eq Salary 
derive instance salaryGen :: Generic Salary _

derive instance titleGen :: Generic Title _
instance titleShow :: Show Title where
  show = genericShow
derive instance titleEq :: Eq Title   

derive instance jobTypeGen :: Generic JobType _
instance jobTypeShow :: Show JobType where
  show = genericShow
derive instance jobTypeEq :: Eq JobType

spec :: Spec Unit
spec =
  describe "Morello.Morello" do
    describe "cherry conversion" do
      it "should fail in case of invalid data" do
        let 
            expected = V (Left (NonEmpty.singleton (FieldInvalid "Salary is too damn low") `NonEmpty.snoc` FieldInvalid "Software Engineering is not a serious profession"))            
            actual = convert invalidPerson
        
        actual `shouldEqual` expected

      it "should be successful in case of valid data" do
        let 
            expected :: PersonOutput
            expected = {
                details : {
                    title : Title "Pilot"
                 , salary : Salary 200000.0
                 , jobType : Worker 
                }
            }
            actual = convert validPerson
        
        actual `shouldEqual` (valid expected)