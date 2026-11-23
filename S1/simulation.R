sim <- function(n, a, b) {
  y <- numeric(n)
  u <- rnorm(n)
  for (i in 1:n) {
    ar <- 0
    for (j in 1:length(a)) if (i > j) ar <- ar + a[j] * y[i - j]
    ma <- 0
    for (j in 1:length(b)) if (i > j) ma <- ma + b[j] * u[i - j]
    y[i] <- ar + ma + u[i]
  }
  y
}

