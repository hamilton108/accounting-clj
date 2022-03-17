{ name = "ps-accounting"
, dependencies = 
  [ "aff"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "console"
  , "dom-indexed"
  , "effect"
  , "either"
  , "foreign-object"
  , "halogen"
  , "maybe"
  , "prelude" 
  , "psci-support"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
