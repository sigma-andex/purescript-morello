module Main where

import Prelude (Unit, (<<<), (>>>))
import Data.Lens (traversed)
import Data.Lens.Getter (view)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (log)
import Morello.Morello.Core (blossom, branch, cherry, (🌱), (🌸), (🍒))
import Morello.Validated (Validated, Validator(..), valid)

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

x :: PersonInput
x =
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

personL = prop (SProxy :: SProxy "person")

addressesL = prop (SProxy :: SProxy "addresses")

zipL = prop (SProxy :: SProxy "zip")

professionL = prop (SProxy :: SProxy "profession")

titleL = prop (SProxy :: SProxy "title")

salaryL = prop (SProxy :: SProxy "salary")

addresses = personL <<< addressesL <<< traversed

titleValidator :: Validator PersonInput String
titleValidator = Validator (view (professionL <<< titleL) >>> valid)

salaryValidator :: Validator PersonInput Number
salaryValidator = Validator (view (professionL <<< salaryL) >>> valid)

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

main :: Effect Unit
main = do
  let
    _ = spy "convert" (convert x)

    _ = spy "convert2" (convert2 x)
  log ""
