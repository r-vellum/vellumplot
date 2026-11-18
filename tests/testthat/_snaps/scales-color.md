# quantitative fill with a discrete color layer is a clear error

    Code
      train(p)
    Condition
      Error in `.colour_channel_type()`:
      ! Layers map color/fill to incompatible variable types.
      x One layer maps fill to a quantitative variable, another maps color to a nominal one.
      i color and fill share one colour scale, so every layer mapping either must use the same kind of variable.
      i Convert one of them (e.g. with `factor()` or `as.numeric()`), or set the scale kind explicitly.

