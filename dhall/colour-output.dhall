let ColourOutput = ./Type/ColourOutput.dhall

let fold
      : ∀(r : Type)
      → ∀ (handler
            : { Always : r
              , Auto : r
              , Never : r
              }
          )
      → ∀(colourOutput : ColourOutput)
      → r

      = λ(r : Type)
      → λ(handler
            : { Always : r
              , Auto : r
              , Never : r
              }
          )
      → λ(colourOutput : ColourOutput)
      → merge handler colourOutput

let -- Usage example:
    --
    -- ```
    --   λ(colourOutput : ColourOutput)
    -- → "ls --color=${toText colourOutput}"
    -- ```
    toText = fold Text
      { Always = "always"
      , Auto = "auto"
      , Never = "never"
      }

in  { fold = fold
    , toText = toText
    }
