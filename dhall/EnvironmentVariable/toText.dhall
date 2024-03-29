-- vim: filetype=dhall

let EnvironmentVariable =
        ./Type.dhall sha256:b8c3c0c4ceb36ba4e6674df5de20ad1d97e120b93b9ce9914a41d0036770dcc4
      ? ./Type.dhall

let fold =
        ./fold.dhall sha256:64154ae1b0c80b0e2870b47224825cdcd90b137ab2935c03a3a897e84bfc6af7
      ? ./fold.dhall

let Text/concat =
      https://prelude.dhall-lang.org/v17.0.0/Text/concat sha256:731265b0288e8a905ecff95c97333ee2db614c39d69f1514cb8eed9259745fc0

let toText
    : ∀(var : EnvironmentVariable) → Text
    = fold
        Text
        Text/concat
        { name = λ(_ : Text) → _ ++ "=", value = λ(_ : Text) → _ }

let example0 = assert : toText { name = "FOO", value = "" } ≡ "FOO="

let example1 = assert : toText { name = "FOO", value = "bar" } ≡ "FOO=bar"

in  toText
