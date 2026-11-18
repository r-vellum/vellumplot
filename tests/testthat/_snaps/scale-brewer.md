# ColorBrewer names match loosely; type/palette index and direction

    Code
      scale_color_brewer(base, type = "qual", palette = 99)
    Condition
      Error in `scale_color_brewer()`:
      ! A numeric `palette` must be a whole number from 1 to 8 for `type = "qual"`.
    Code
      scale_color_brewer(base, palette = "Set1", direction = 2)
    Condition
      Error in `scale_color_brewer()`:
      ! `direction` must be 1 or -1.

