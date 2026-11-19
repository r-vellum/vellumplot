# base_size scales a complete theme (VP-10)

    Code
      theme_gray(vplot(mtcars), base_size = -1)
    Condition
      Error in `theme_gray()`:
      ! `base_size` must be a single positive number (points).

# rel() resolves against the parent element (VP-10)

    Code
      rel("a")
    Condition
      Error in `rel()`:
      ! `x` must be a single positive number.

