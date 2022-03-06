{ name = "ps-accounting"
, dependencies = 
    [ "console"
    , "effect"
    , "halogen"
    , "web-dom"
    , "maybe"
    , "psci-support"
    , "prelude" 
    , "dom-indexed"
    , "web-uievents"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
