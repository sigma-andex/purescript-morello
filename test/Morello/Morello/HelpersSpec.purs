module Morello.Morello.HelpersSpec where

import Data.Newtype (class Newtype)
import Prelude (class Eq, class Show, Unit, (>>>))
import Morello.Morello
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type PersonInput
  = { title :: String
    , id :: Int
    }

newtype Title
  = Title String

derive instance titleNT :: Newtype Title _

derive newtype instance titleShow :: Show Title

derive newtype instance titleEq :: Eq Title

type PersonOutput
  = { title :: Title, id :: Int }

convert :: PersonInput -> Validated PersonOutput
convert =
  branch
    >>> cherry
        { title:
            pick  (key :: _ "title") (as Title) :: Pick PersonInput Title
        , id:
            pick  (key :: _ "id") asIs :: Pick PersonInput Int
        }
    >>> blossom

validPerson :: PersonInput
validPerson =
  { title: "Pilot"
  , id: 10000
  }

spec :: Spec Unit
spec =
  describe "Morello.Morello" do
    describe "cherry basic validations" do
      it "should validate correctly" do
        let
          expected :: PersonOutput
          expected =
            { title: Title "Pilot"
            , id: 10000
            }

          actual = convert validPerson
        actual `shouldEqual` (valid expected)
