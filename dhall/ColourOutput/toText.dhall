-- vim: filetype=dhall

let fold =
        ./fold.dhall sha256:a020d0af0e820105754b9ebb4d891d6fbed4aef8511a2a148cc1303261ca5388
      ? ./fold.dhall

let toText = fold Text { Always = "always", Auto = "auto", Never = "never" }

in  toText
