# ---------------------------------------------------------------------------
# Circle packing (port of d3-hierarchy's pack primitives, base R only).
#
# Two pure helpers used by circular hierarchy marks (circle-pack / bubble):
#   .pack_siblings(r)     - front-chain packing of sibling circles ("packSiblings")
#   .pack_enclose(x,y,r)  - smallest enclosing circle ("packEnclose", Welzl)
#
# Faithful translation of d3-hierarchy. The front chain is a doubly-linked list
# represented with integer next/prev index vectors (node id == circle index),
# not per-node environments. The packEnclose random shuffle is dropped: the move-
# to-front result is deterministic and order only affects worst-case cost, not
# correctness, and n is small here.
# ---------------------------------------------------------------------------

# Place circle c (radius cr) tangent to placed circles b and a. Returns c(x, y).
# Param order matches d3's place(b, a, c): dx/dy are measured from a to b.
.pack_place <- function(bx, by, br, ax, ay, ar, cr) {
  dx <- bx - ax
  dy <- by - ay
  d2 <- dx * dx + dy * dy
  if (d2 > 0) {
    a2 <- (ar + cr)^2
    b2 <- (br + cr)^2
    if (a2 > b2) {
      x <- (d2 + b2 - a2) / (2 * d2)
      y <- sqrt(max(0, b2 / d2 - x * x))
      cx <- bx - x * dx - y * dy
      cy <- by - x * dy + y * dx
    } else {
      x <- (d2 + a2 - b2) / (2 * d2)
      y <- sqrt(max(0, a2 / d2 - x * x))
      cx <- ax + x * dx - y * dy
      cy <- ay + x * dy + y * dx
    }
  } else {
    cx <- ax + cr
    cy <- ay
  }
  c(cx, cy)
}

# Do circles a and b overlap (with a tiny tolerance)?
.pack_intersects <- function(ax, ay, ar, bx, by, br) {
  dr <- ar + br - 1e-6
  dx <- bx - ax
  dy <- by - ay
  dr > 0 && dr * dr > dx * dx + dy * dy
}

# Pack sibling circles with radii r; centres translated so the enclosing circle
# sits at the origin. Returns list(x, y).
.pack_siblings <- function(r) {
  n <- length(r)
  if (n == 0) {
    return(list(x = numeric(0), y = numeric(0)))
  }
  xs <- numeric(n)
  ys <- numeric(n)

  # First circle at the origin.
  xs[1] <- 0
  ys[1] <- 0
  if (n == 1) {
    return(list(x = 0, y = 0))
  }

  # Second circle to the right; the two already straddle the origin, but run
  # them through enclose for a uniform origin-centred result.
  xs[1] <- -r[2]
  ys[1] <- 0
  xs[2] <- r[1]
  ys[2] <- 0
  if (n == 2) {
    e <- .pack_enclose(xs, ys, r)
    return(list(x = xs - e$x, y = ys - e$y))
  }

  # Third circle: place(b = circle2, a = circle1, c = circle3).
  cc <- .pack_place(xs[2], ys[2], r[2], xs[1], ys[1], r[1], r[3])
  xs[3] <- cc[1]
  ys[3] <- cc[2]

  # Front chain over circle indices (node id == circle index).
  nxt <- integer(n)
  prv <- integer(n)
  # a=1, b=2, c=3 arranged as a<->b<->c<->a.
  nxt[1] <- 2L
  prv[3] <- 2L
  nxt[2] <- 3L
  prv[1] <- 3L
  nxt[3] <- 1L
  prv[2] <- 1L

  # score(node m): squared distance of the m/next(m) weighted midpoint to origin.
  sc <- function(m) {
    nn <- nxt[m]
    ab <- r[m] + r[nn]
    dx <- (xs[m] * r[nn] + xs[nn] * r[m]) / ab
    dy <- (ys[m] * r[nn] + ys[nn] * r[m]) / ab
    dx * dx + dy * dy
  }

  a <- 1L
  b <- 2L
  i <- 4L
  while (i <= n) {
    # Place the next circle: place(a, b, c) -> place(b = a, a = b, c).
    cc <- .pack_place(xs[a], ys[a], r[a], xs[b], ys[b], r[b], r[i])
    xs[i] <- cc[1]
    ys[i] <- cc[2]
    ci <- i

    # Walk the chain outward from b (forward) and a (backward) for the closest
    # intersecting circle. On a hit, splice it in and retry this circle.
    j <- nxt[b]
    k <- prv[a]
    sj <- r[b]
    sk <- r[a]
    spliced <- FALSE
    repeat {
      if (sj <= sk) {
        if (.pack_intersects(xs[j], ys[j], r[j], xs[ci], ys[ci], r[ci])) {
          b <- j
          nxt[a] <- b
          prv[b] <- a
          spliced <- TRUE
          break
        }
        sj <- sj + r[j]
        j <- nxt[j]
      } else {
        if (.pack_intersects(xs[k], ys[k], r[k], xs[ci], ys[ci], r[ci])) {
          a <- k
          nxt[a] <- b
          prv[b] <- a
          spliced <- TRUE
          break
        }
        sk <- sk + r[k]
        k <- prv[k]
      }
      if (j == nxt[k]) {
        break
      }
    }
    if (spliced) {
      next # retry the same circle with the updated a/b
    }

    # Insert c between a and b.
    prv[ci] <- a
    nxt[ci] <- b
    nxt[a] <- ci
    prv[b] <- ci
    b <- ci

    # Recompute the pair closest to the centroid; walk the whole chain.
    aa <- sc(a)
    cnode <- ci
    repeat {
      cnode <- nxt[cnode]
      if (cnode == b) {
        break
      }
      ca <- sc(cnode)
      if (ca < aa) {
        a <- cnode
        aa <- ca
      }
    }
    b <- nxt[a]
    i <- i + 1L
  }

  # Enclose the front-chain circles and translate all circles to the origin.
  chain <- b
  cnode <- b
  repeat {
    cnode <- nxt[cnode]
    if (cnode == b) {
      break
    }
    chain <- c(chain, cnode)
  }
  e <- .pack_enclose(xs[chain], ys[chain], r[chain])
  list(x = xs - e$x, y = ys - e$y)
}

# --- smallest enclosing circle (Welzl move-to-front) -----------------------
# Circles are numeric triples c(x, y, r); bases B are lists of such triples.

.enc_encloses_weak <- function(a, b) {
  dr <- a[3] - b[3] + max(a[3], b[3], 1) * 1e-9
  dx <- b[1] - a[1]
  dy <- b[2] - a[2]
  dr > 0 && dr * dr > dx * dx + dy * dy
}

.enc_encloses_not <- function(a, b) {
  dr <- a[3] - b[3]
  dx <- b[1] - a[1]
  dy <- b[2] - a[2]
  dr < 0 || dr * dr < dx * dx + dy * dy
}

.enc_encloses_weak_all <- function(a, B) {
  for (bb in B) {
    if (!.enc_encloses_weak(a, bb)) {
      return(FALSE)
    }
  }
  TRUE
}

.enc_basis1 <- function(a) a

.enc_basis2 <- function(a, b) {
  x1 <- a[1]
  y1 <- a[2]
  r1 <- a[3]
  x2 <- b[1]
  y2 <- b[2]
  r2 <- b[3]
  x21 <- x2 - x1
  y21 <- y2 - y1
  r21 <- r2 - r1
  l <- sqrt(x21 * x21 + y21 * y21)
  c(
    (x1 + x2 + x21 / l * r21) / 2,
    (y1 + y2 + y21 / l * r21) / 2,
    (l + r1 + r2) / 2
  )
}

.enc_basis3 <- function(a, b, c) {
  x1 <- a[1]
  y1 <- a[2]
  r1 <- a[3]
  x2 <- b[1]
  y2 <- b[2]
  r2 <- b[3]
  x3 <- c[1]
  y3 <- c[2]
  r3 <- c[3]
  a2 <- x1 - x2
  a3 <- x1 - x3
  b2 <- y1 - y2
  b3 <- y1 - y3
  c2 <- r2 - r1
  c3 <- r3 - r1
  d1 <- x1 * x1 + y1 * y1 - r1 * r1
  d2 <- d1 - x2 * x2 - y2 * y2 + r2 * r2
  d3 <- d1 - x3 * x3 - y3 * y3 + r3 * r3
  ab <- a3 * b2 - a2 * b3
  # Collinear centres give ab == 0: there is no proper three-circle basis, and
  # dividing by it would yield Inf/NaN coordinates that poison the pack. Fall back
  # to the two-circle basis of whichever pair weakly encloses the third circle.
  if (ab == 0) {
    for (pr in list(c(1L, 2L), c(1L, 3L), c(2L, 3L))) {
      circs <- list(a, b, c)
      cand <- .enc_basis2(circs[[pr[1]]], circs[[pr[2]]])
      if (.enc_encloses_weak_all(cand, circs)) {
        return(cand)
      }
    }
    return(.enc_basis2(a, c))
  }
  xa <- (b2 * d3 - b3 * d2) / (ab * 2) - x1
  xb <- (b3 * c2 - b2 * c3) / ab
  ya <- (a3 * d2 - a2 * d3) / (ab * 2) - y1
  yb <- (a2 * c3 - a3 * c2) / ab
  aa <- xb * xb + yb * yb - 1
  bb <- 2 * (r1 + xa * xb + ya * yb)
  cc <- xa * xa + ya * ya - r1 * r1
  r <- -(if (abs(aa) > 1e-6) {
    (bb + sqrt(bb * bb - 4 * aa * cc)) / (2 * aa)
  } else {
    cc / bb
  })
  c(x1 + xa + xb * r, y1 + ya + yb * r, r)
}

.enc_basis <- function(B) {
  switch(
    length(B),
    .enc_basis1(B[[1]]),
    .enc_basis2(B[[1]], B[[2]]),
    .enc_basis3(B[[1]], B[[2]], B[[3]])
  )
}

.enc_extend_basis <- function(B, p) {
  if (.enc_encloses_weak_all(p, B)) {
    return(list(p))
  }
  nb <- length(B)
  for (i in seq_len(nb)) {
    if (
      .enc_encloses_not(p, B[[i]]) &&
        .enc_encloses_weak_all(.enc_basis2(B[[i]], p), B)
    ) {
      return(list(B[[i]], p))
    }
  }
  if (nb >= 2) {
    for (i in seq_len(nb - 1)) {
      for (j in (i + 1):nb) {
        if (
          .enc_encloses_not(.enc_basis2(B[[i]], B[[j]]), p) &&
            .enc_encloses_not(.enc_basis2(B[[i]], p), B[[j]]) &&
            .enc_encloses_not(.enc_basis2(B[[j]], p), B[[i]]) &&
            .enc_encloses_weak_all(.enc_basis3(B[[i]], B[[j]], p), B)
        ) {
          return(list(B[[i]], B[[j]], p))
        }
      }
    }
  }
  cli::cli_abort(
    ".enc_extend_basis: no enclosing basis found (internal error)."
  )
}

# Smallest circle enclosing the input circles. Returns list(x, y, r).
.pack_enclose <- function(x, y, r) {
  n <- length(r)
  if (n == 0) {
    return(list(x = 0, y = 0, r = 0))
  }
  circles <- lapply(seq_len(n), function(k) c(x[k], y[k], r[k]))
  i <- 1L
  B <- list()
  e <- NULL
  while (i <= n) {
    p <- circles[[i]]
    if (!is.null(e) && .enc_encloses_weak(e, p)) {
      i <- i + 1L
    } else {
      B <- .enc_extend_basis(B, p)
      e <- .enc_basis(B)
      i <- 1L
    }
  }
  list(x = e[[1]], y = e[[2]], r = e[[3]])
}
