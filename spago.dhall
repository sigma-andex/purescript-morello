{ name = "morello"
, dependencies =
  [ "barlow-lens"
  , "effect"
  , "heterogeneous"
  , "heterogeneous-extrablatt"
  , "profunctor-lenses"
  , "spec"
  , "spec-discovery"
  , "validation"
  , "aff"
  , "arrays"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-morello.git"
}
