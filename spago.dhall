{ name = "purescript-morello"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "heterogeneous"
  , "profunctor-lenses"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "strings"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-morello.git"
}
