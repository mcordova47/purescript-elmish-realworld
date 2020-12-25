{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-elmish-realworld"
, dependencies = [ "console", "effect", "elmish", "elmish-html", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
