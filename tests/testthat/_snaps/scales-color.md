# one channel mapped to mixed variable types is a clear error

    Code
      train(p)
    Condition
      Error in `.colour_channel_type()`:
      ! Layers map color to incompatible variable types.
      x One layer maps it to a quantitative variable, another to a nominal one.
      i Layers share one color scale, so every layer mapping it must use the same kind of variable.
      i Convert one of them (e.g. with `factor()` or `as.numeric()`), or set the scale type explicitly.

