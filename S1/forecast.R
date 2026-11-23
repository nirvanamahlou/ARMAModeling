pred <- function(m, y, e, h = 5) {
  a <- m$a
  b <- m$b
  p <- length(a)
  q <- length(b)
  n <- length(y)

  f <- numeric(h)

  for (i in 1:h) {
    ar <- 0
    for (j in 1:p) {
      if (i > j) {
        ar <- ar + a[j] * f[i - j]
      } else if (n + i - j > 0) {
        ar <- ar + a[j] * y[n + i - j]
      }
    }
    ma <- 0
    for (j in 1:q) {
      idx <- n + i - j
      if (idx <= length(e) && idx > 0) {
        ma <- ma + b[j] * e[idx]
      }
    }
    f[i] <- ar + ma
  }

  return(f)
}

