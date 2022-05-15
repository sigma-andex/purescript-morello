let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220513/packages.dhall
        sha256:1ed784f37ae6131d99acd542d058d5ce39954ccaacc3adba5cc7cf1549d2bffa

in  upstream
  with heterogeneous-extrablatt =
    { dependencies = [ "heterogeneous", "prelude", "record" ]
    , version = "v0.2.1"
    , repo =
        "https://github.com/sigma-andex/purescript-heterogeneous-extrablatt.git"
    }
  with barlow-lens =
    { dependencies =
      [ "either"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "newtype"
      , "prelude"
      , "profunctor"
      , "profunctor-lenses"
      , "tuples"
      , "typelevel-prelude"
      ]
    , version = "v0.9.0"
    , repo = "https://github.com/sigma-andex/purescript-barlow-lens.git"
    }
