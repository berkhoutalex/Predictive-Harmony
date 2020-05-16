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
  , "midi"
  , "numbers"
  , "options"
  , "psci-support"
  , "purescript-school-of-music"
  , "react-basic-hooks"
  , "soundfonts"
  , "spec"
  , "string-parsers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
