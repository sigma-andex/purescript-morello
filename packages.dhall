let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.15/src/packages.dhall
        sha256:b1c6d06132b7cbf1e93b1e5343044fba1604b50bfbe02d8f80a3002e71569c59

in  upstream
  with metadata.version = "v0.15.0-alpha-05"
  with spec =
    { repo = "https://github.com/purescript-spec/purescript-spec.git"
    , version = "v6.0.0"
    , dependencies =
      [ "aff"
      , "ansi"
      , "avar"
      , "console"
      , "exceptions"
      , "foldable-traversable"
      , "fork"
      , "now"
      , "pipes"
      , "prelude"
      , "strings"
      , "transformers"
      ]
    }
  with spec-discovery =
    { repo = "https://github.com/purescript-spec/purescript-spec-discovery.git"
    , version = "v7.0.0"
    , dependencies =
      [ "aff"
      , "aff-promise"
      , "arrays"
      , "console"
      , "effect"
      , "foldable-traversable"
      , "node-fs"
      , "prelude"
      , "spec"
      ]
    }
  with variant =
    { dependencies =
      [ "enums"
      , "lists"
      , "maybe"
      , "partial"
      , "prelude"
      , "record"
      , "tuples"
      , "unsafe-coerce"
      ]
    , repo =
        "https://github.com/working-group-purescript-es/purescript-variant.git"
    , version = "v0.15.0-update"
    }
  with heterogeneous =
    { dependencies =
      [ "either", "functors", "prelude", "record", "tuples", "variant" ]
    , repo = "https://github.com/natefaubion/purescript-heterogeneous.git"
    , version = "v0.5.1"
    }
  with heterogeneous-extrablatt =
    { dependencies =
      [ "console", "effect", "heterogeneous", "spec", "spec-discovery" ]
    , version = "v0.1.0"
    , repo =
        "https://github.com/sigma-andex/purescript-heterogeneous-extrablatt.git"
    }
