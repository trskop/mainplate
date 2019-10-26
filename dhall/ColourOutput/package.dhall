-- vim: filetype=dhall

let ColourOutput =
        ./Type sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ./Type

let toText =
        ./toText sha256:51d22acdc9b32f757e9170ae7c2cf244ce0488aa839dada9ee38bb3f1ee4d7bf
      ? ./toText

let show =
        ./show sha256:185d165dc7953ca951e11a3815853cbb9c81a2a555ad3f959735094f98a346b1
      ? ./show

let test = λ(_ : ColourOutput) → { t = toText _, s = show _ }

let consistency0 =
      assert : test ColourOutput.Always ≡ { t = "always", s = "Always" }

let consistency1 = assert : test ColourOutput.Auto ≡ { t = "auto", s = "Auto" }

let consistency2 =
      assert : test ColourOutput.Never ≡ { t = "never", s = "Never" }

in  { Type =
        ColourOutput
    , fold =
          ./fold sha256:a020d0af0e820105754b9ebb4d891d6fbed4aef8511a2a148cc1303261ca5388
        ? ./fold
    , toText = toText
    , show = show
    }
