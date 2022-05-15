{ name = "morello"
, dependencies =
  [ "barlow-lens"
  , "heterogeneous"
  , "heterogeneous-extrablatt"
  , "profunctor-lenses"
  , "validation"
  , "arrays"
  , "foldable-traversable"
  , "newtype"
  , "prelude"
  , "profunctor"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs"]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-morello.git"
}
