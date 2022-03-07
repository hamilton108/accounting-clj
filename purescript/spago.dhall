{ name = "ps-accounting"
, dependencies = 
    [ "aff"
    , "affjax"
    , "argonaut-codecs"
    , "argonaut-core"
    , "console"
    , "dom-indexed"
    , "effect"
    , "either"
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
