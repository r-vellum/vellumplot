# a continuous fill varying along an area or ribbon errors

    Code
      compile(mark_area(vplot(d), x = x, y = y, fill = v))
    Condition
      Error:
      ! `mark_area()` can't vary fill continuously along one area.
      i A filled area is a single polygon, so it takes one fill per series.
      i Map a discrete variable, set a group with one value per group, or use a constant fill.
    Code
      compile(mark_ribbon(vplot(d), x = x, ymin = lo, ymax = y, fill = v))
    Condition
      Error:
      ! `mark_ribbon()` can't vary fill continuously along one ribbon.
      i A filled ribbon is a single polygon, so it takes one fill per series.
      i Map a discrete variable, set a group with one value per group, or use a constant fill.

