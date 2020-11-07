-- vim: filetype=dhall

let EnvironmentVariable
    : Type
    = { name : Text, value : Text }

in  EnvironmentVariable
