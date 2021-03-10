# Quick start

```purescript
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

-- now let's start converting! 
convert :: PersonInput -> Validated PersonOutput
convert =
  branch -- start with a branch
    >>> cherry { -- then start cherry picking
            details : { -- by defining how your output format should look like
                title: 
                    -- then pick data from your input by zooming in using the lens...
                    pick (professionL |> titleL ) validateTitle :: Pick PersonInput Title
              , salary:
                    -- ...and validate using your validator
                    pick (professionL |> salaryL ) validateSalary :: Pick PersonInput Salary
              -- you can also set constant data
              , jobType : Worker
            }
        }
    >>> blossom -- finish up

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
```

See also [MinimalSpec](./test/Morello/Morello/MinimalSpec.purs).
