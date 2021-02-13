{ name = "purescript-morello"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "generics-rep"
  , "heterogeneous"
  , "profunctor-lenses"
  , "psci-support"
  , "strings"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
