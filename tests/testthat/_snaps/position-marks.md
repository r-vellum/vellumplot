# a position a mark does not support is an error naming the mark

    Code
      mark_errorbar(vplot(summ), x = cat, ymin = m, ymax = m, position = "stack")
    Condition
      Error in `mark_errorbar()`:
      ! `mark_errorbar()` does not support `position = "stack"`.
      i It supports: "identity", "nudge", "dodge", and "dodge2".

---

    Code
      mark_text(vplot(summ), x = cat, y = m, label = m, position = "dodgy")
    Condition
      Error in `mark_text()`:
      ! `mark_text()` does not support `position = "dodgy"`.
      i It supports: "identity", "nudge", "stack", "fill", "dodge", "dodge2", and "jitter".
      x "dodgy" is not a position adjustment.

