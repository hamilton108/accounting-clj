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
    , "tuples"
    , "either"
    , "foreign-object"
    , "halogen"
    , "maybe"
    , "prelude" 
    , "psci-support"
    , "web-dom"
    , "web-events"
    , "web-uievents"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
