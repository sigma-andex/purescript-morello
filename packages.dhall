let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0/packages.dhall sha256:710b53c085a18aa1263474659daa0ae15b7a4f453158c4f60ab448a6b3ed494e

in  upstream
  with heterogeneous =
    { dependencies =
      [ "prelude", "record", "tuples", "functors", "variant", "either" ]
    , repo = "https://github.com/JordanMartinez/purescript-heterogeneous.git"
    , version = "polykindsUpdate"
    }
  with variant =
    { dependencies =
      [ "prelude"
      , "tuples"
      , "unsafe-coerce"
      , "partial"
      , "maybe"
      , "lists"
      , "record"
      , "enums"
      ]
    , repo = "https://github.com/JordanMartinez/purescript-variant.git"
    , version = "polykindsUpdate"
    }
  with spec =
    { dependencies =
      [ "avar"
      , "console"
      , "aff"
      , "exceptions"
      , "strings"
      , "prelude"
      , "transformers"
      , "foldable-traversable"
      , "pipes"
      , "ansi"
      , "fork"
      , "now"
      ]
    , repo = "https://github.com/purescript-spec/purescript-spec.git"
    , version = "v5.0.0"
    }
