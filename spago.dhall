{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "datetime-parsing"
, dependencies = [ "arrays"
                 , "datetime"
                 , "either"
                 , "enums"
                 , "foldable-traversable"
                 , "integers"
                 , "lists"
                 , "maybe"
                 , "numbers"
                 , "parsing"
                 , "prelude"
                 , "strings"
                 , "unicode" ]
, license = "BSD-3-Clause"
, packages = ./packages.dhall
, repository = "https://github.com/flounders/purescript-datetime-parsing"
, sources = [ "src/**/*.purs" ]
}
