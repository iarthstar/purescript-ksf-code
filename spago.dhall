{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-react-basic-user-details"
, dependencies =
    [ "console"
    , "debug"
    , "effect"
    , "foreign"
    , "foreign-generic"
    , "generics-rep"
    , "nullable"
    , "prelude"
    , "psci-support"
    , "react-basic"
    , "simple-json"
    , "strings"
    ]
, packages =
    ./packages.dhall
}
