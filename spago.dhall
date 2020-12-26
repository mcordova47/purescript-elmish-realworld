{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-elmish-realworld"
, dependencies = [ "affjax", "argonaut-generic", "console", "effect", "elmish", "elmish-html", "formatters", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
