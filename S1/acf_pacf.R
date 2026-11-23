
acf_ <- function(y, m = 40) {
  n <- length(y)
  mu <- sum(y) / n
  g0 <- sum((y - mu)^2) / n
  v <- numeric(m + 1)
  for (h in 0:m) {
    cov <- sum((y[1:(n-h)] - mu) * (y[(1+h):n] - mu)) / n
    v[h+1] <- cov / g0
  }
  v
}

pacf_ <- function(y, m = 40) {
  ac <- acf_(y, m)
  phi <- matrix(0, m, m)
  res <- numeric(m + 1)
  var_err <- numeric(m + 1)
  res[1] <- 1
  var_err[1] <- ac[1]
  for (k in 1:m) {
    num <- ac[k + 1]
    if (k > 1) {
      num <- num - sum(phi[k - 1, 1:(k - 1)] * ac[k:2])
    }
    phi[k, k] <- num / var_err[k]
    if (k > 1) {
      for (j in 1:(k - 1)) {
        phi[k, j] <- phi[k - 1, j] - phi[k, k] * phi[k - 1, k - j]
      }
    }
    var_err[k + 1] <- var_err[k] * (1 - phi[k, k]^2)
    res[k + 1] <- phi[k, k]
  }
  res
}


