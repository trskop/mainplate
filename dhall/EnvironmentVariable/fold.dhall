-- vim: filetype=dhall

let EnvironmentVariable =
        ./Type.dhall sha256:b8c3c0c4ceb36ba4e6674df5de20ad1d97e120b93b9ce9914a41d0036770dcc4
      ? ./Type.dhall

let Monoid =
      https://prelude.dhall-lang.org/v17.0.0/Monoid sha256:26fafa098600ef7a54ef9dba5ada416bbbdd21df1af306c052420c61553ad4af

let fold
    : ∀(r : Type) →
      ∀(monoid : Monoid r) →
      ∀(convert : { name : Text → r, value : Text → r }) →
      ∀(var : EnvironmentVariable) →
        r
    = λ(r : Type) →
      λ(monoid : Monoid r) →
      λ(convert : { name : Text → r, value : Text → r }) →
      λ(var : EnvironmentVariable) →
        monoid [ convert.name var.name, convert.value var.value ]

in  fold
