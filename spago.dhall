{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "panepon"
, dependencies =
    [ "console"
    , "control"
    , "effect"
    , "foldable-traversable"
    , "halogen"
    , "lists"
    , "maybe"
    , "psci-support"
    , "random"
    , "tuples"
    , "unfoldable"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
