
fit <- function(y, p, q) {
  mod <- arima(y, order = c(p, 0, q), method = "ML")
  if (is.null(mod)) {
    return(NULL)
  }
  ar_c <- if (p > 0) as.numeric(mod$coef[grep("^ar", names(mod$coef))])
  ma_c <- if (q > 0) as.numeric(mod$coef[grep("^ma", names(mod$coef))])
  aic <- AIC(mod)
  list(
    a = ar_c,
    b = ma_c,
    sigma2 = mod$sigma2,
    loglik = mod$loglik,
    AIC = aic
  )
}

fa<- function(y, p, q) {
  mod <- arima(y, order = c(p,0, q))
  return(mod)
}

