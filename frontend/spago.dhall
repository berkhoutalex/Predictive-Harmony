{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pred-harm"
, dependencies =
    [ "aff-promise"
    , "affjax"
    , "argonaut"
    , "argonaut-core"
    , "console"
    , "debug"
    , "effect"
    , "formatters"
    , "js-date"
    , "js-timers"
    , "lists"
    , "numbers"
    , "options"
    , "psci-support"
    , "react-basic-hooks"
    , "spec"
    , "string-parsers"
    , "tuples"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
