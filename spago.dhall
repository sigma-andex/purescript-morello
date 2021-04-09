{ name = "morello"
, dependencies =
  [ "barlow-lens"
  , "console"
  , "debug"
  , "effect"
  , "heterogeneous"
  , "heterogeneous-extrablatt"
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
