# purescript-morello 🌸

A purescript library for cherry-picking 🍒 your data.

The goal of this library is to make it super simple to validate input data and transform it into output data.

## tl;dr

```purescript
import Prelude
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Morello.Morello

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
titleValidator :: Validate String
titleValidator "Software Engineer" = invalid (FieldInvalid "Software Engineering is not a serious profession")
titleValidator s = valid s

salaryValidator :: Validate Number
salaryValidator n 
    | n > 50000.0 = valid n
salaryValidator n = invalid (FieldInvalid "Salary is too damn low")

-- some necessary type information
pickV = pickP (Proxy :: Proxy PersonInput)


-- define your convert function
convert :: PersonInput -> Validated PersonOutput
convert =
  branch
    >>> cherry { 
            details : {
                title: 
                    pickV (professionL |> titleL |> validateOverL Title titleValidator)
            , salary:
                    pickV (professionL |> salaryL |> validateOverL Salary salaryValidator)
                    
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

first = convert invalidPerson
-- invalid ((NonEmptyArray [(FieldInvalid "Salary is too damn low"),(FieldInvalid "Software Engineering is not a serious profession")]))

second = convert validPerson
-- pure ({ details: { jobType: Worker, salary: (Salary 200000.0), title: (Title "Pilot") } })
```

See also [MinimalSpec](./src/test/Morello/Morello/MinimalSpec.purs).

## Usage 



## Connecting the dots 

This work is merely connecting the dots of the amazing Purescript language and it's awesome ecosystem. A special shout-out to the contributors & maintainers of the [`profunctor-lenses`](https://github.com/purescript-contrib/purescript-profunctor-lenses), [`heterogenous`](https://github.com/natefaubion/purescript-heterogeneous/) and [`validation`](https://github.com/purescript/purescript-validation) libraries. This library wouldn't have been possible without these.