-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:b8c3c0c4ceb36ba4e6674df5de20ad1d97e120b93b9ce9914a41d0036770dcc4
    ? ./Type.dhall
, fold =
      ./fold.dhall sha256:64154ae1b0c80b0e2870b47224825cdcd90b137ab2935c03a3a897e84bfc6af7
    ? ./fold.dhall
, toText =
      ./toText.dhall sha256:14b6ceff46e7ec8cd1846d2f50ba366efb3e623f6d92e7543aabaab1ac265f34
    ? ./toText.dhall
}
