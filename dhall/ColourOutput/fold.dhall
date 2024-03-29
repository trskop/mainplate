-- vim: filetype=dhall
--
-- Example:
--
-- ```
--   fold
--     (List Text)
--     { Always = [ "--color-output" ]
--     , Auto = [] : List Text
--     , Never = [ "--monochrome-output" ]
--     }
-- : ColourOutput → List Text
-- ```

let ColourOutput =
        ./Type.dhall sha256:0ef91a3f044406ee80fc20f26127b448a0e98f46c46ec024457023d2aded8543
      ? ./Type.dhall

let fold
    : ∀(r : Type) →
      ∀(handler : { Always : r, Auto : r, Never : r }) →
      ∀(colourOutput : ColourOutput) →
        r
    = λ(r : Type) →
      λ(handler : { Always : r, Auto : r, Never : r }) →
      λ(colourOutput : ColourOutput) →
        merge handler colourOutput

in  fold
