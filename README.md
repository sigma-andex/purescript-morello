# purescript-morello 🌸

A purescript library for cherry-picking 🍒 your data.

The goal of this library is to make it super simple to validate input data and transform it into output data.

## tl;dr 
Transform & validate your input data in a declarative way.

```purescript
-- Given an input model...
type PersonInput = { profession :: { title :: String, salary :: Number } }

-- ...and an output model...
type PersonOutput = { details :: { title :: Title, salary :: Salary, jobType :: JobType } }

-- ..write some validators...
validateTitle :: Validate String Title
validateTitle "Software Engineer" = invalid (FieldInvalid "Software Engineering is not a serious profession")
validateTitle s = valid (Title s)

validateSalary :: Validate Number Salary
validateSalary n 
    | n > 50000.0 = valid (Salary n)
validateSalary n = invalid (FieldInvalid "Salary is too damn low")

-- ...and create a conversion in a declarative way.. 
convert :: PersonInput -> Validated PersonOutput
convert =
  branch 
    >>> cherry 
        -- ...by defining how your output data will look like...
        { 
            details : { 
                title: 
                    -- ...by picking data from the input... 
                    pick (professionL |> titleL ) validateTitle :: Validator PersonInput Title
              , salary:
                    -- ...and validating it using validators.
                    pick (professionL |> salaryL ) validateSalary :: Validator PersonInput Salary
              , jobType : Worker
            }
        }
    >>> blossom 
```
## Quick start

See [quick start](./docs/quickstart.md)

## Features

- Declarative data conversion
- Automatic, parallel error accumulation
- Composable 

## Usage 

TBD

## Connecting the dots 

This work is merely connecting the dots of the amazing Purescript language and its awesome ecosystem. A special shout-out to the contributors & maintainers of the [`profunctor-lenses`](https://github.com/purescript-contrib/purescript-profunctor-lenses), [`heterogeneous`](https://github.com/natefaubion/purescript-heterogeneous/) and [`validation`](https://github.com/purescript/purescript-validation) libraries. This library wouldn't have been possible without these.
