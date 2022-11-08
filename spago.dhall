{ name = "morello"
, dependencies =
  [ "arrays"
  , "barlow-lens"
  , "foldable-traversable"
  , "heterogeneous"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "record"
  , "record-studio"
  , "tuples"
  , "typelevel-prelude"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-morello.git"
}
