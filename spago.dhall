{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "eventstore"
, dependencies =
    [ "aff"
    , "argonaut"
    , "argonaut-codecs"
    , "console"
    , "effect"
    , "foldable-traversable"
    , "milkis"
    , "psci-support"
    , "uuid"
    , "b64"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
