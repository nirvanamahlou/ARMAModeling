install.packages("tseries")
install.packages("e1071")
install.packages("Kendall")
install.packages("forecast")
library(forecast)
library(tseries) 
library(e1071)
library(Kendall)
setwd("C:/Users/E-BUY/Documents/myARMA/R")
files <- list.files("R", pattern="\\.R$")
for(f in files){ 
  source(f)
}
y_random <- sim(500, c(0.6,-0.3), c(0.5,-0.4))
print(y_random)
plot.ts(y_random, main="φ=(0.6,−0.3), θ=(0.5,−0.4)", ylab=expression(X[t]), col="blue")
seeds <- c(42, 101, 202, 303, 404)
n <- 500
a <- c(0.6, -0.3)
b <- c(0.5, -0.4)
list <- list()
for(s in seeds) {
  set.seed(s)
  y <- sim(n, a, b)
  list[[paste0(s)]] <- ts(y)
}
plot.ts(list[["42"]], main="42", ylab=expression(X[t]), col="blue")
plot.ts(list[["101"]], main="101" ,ylab=expression(X[t]), col="blue")
plot.ts(list[["202"]], main="202" ,ylab=expression(X[t]), col="blue")
plot.ts(list[["303"]], main="303" ,ylab=expression(X[t]), col="blue")
plot.ts(list[["404"]], main="404" ,ylab=expression(X[t]), col="blue")
a <- c(list("no_seed" = y), list)
for(i in names(a)) {
  y <- a[[i]]
  acfs <- acf_(y, m = 10)
  pacfs <- pacf_(y, m = 10)
  sk <- skewness(y)
  ku <- kurtosis(y)
  print(paste("Seed:", i))
  print(paste("Mean:", mean(y)))
  print(paste("Variance:", var(y)))
  print(paste("ACF1:", acfs[2]))
  print(paste("PACF1:", pacfs[2]))
  print(paste("Skewness:", sk))
  print(paste("Kurtosis:", ku))
}
sp <- list()
for(i in names(a)) {
  x <- a[[i]]
  n <- length(x)
  sd_seq <- seq(1, 5, length.out = n)
  hetero_noise <- rnorm(n, mean = 0, sd = sd_seq)
  x_hetero <- x + hetero_noise
  sp[[paste0(i, "_hetero")]] <- ts(x_hetero)
  trend <- seq(0, 10, length.out = n)
  x_trend <- x + trend
  sp[[paste0(i, "_trend")]] <- ts(x_trend)
}
com <- c(a, sp)
for(i in names(com)) {
  y <- com[[i]]
  acfs <- acf_(y, m=40)
  pacfs <- pacf_(y, m=40)
  barplot(acfs, main=paste("ACF:", i), ylab="ACF", col="steelblue", ylim=c(-1, 1))
  abline(h=0, col="gray")
  barplot(pacfs, main=paste("PACF:", i), ylab="PACF", col="darkorange", ylim=c(-1, 1))
  abline(h=0, col="gray")
}
for(i in names(sp)) {
  plot.ts(sp[[i]], main=i, col=if(grepl("hetero", i)) "firebrick" else if(grepl("trend", i)) "darkgreen" else "blue", ylab=expression(X[t]))
  abline(h=0, col="gray")
}
for(i in names(com)) {
  cat(i)
  x <- com[[i]]
  cat("\n")
  cat("ADF:\n")
  print(adf.test(x))
  cat("PP:\n")
  print(pp.test(x))
  cat("Mann-Kendall:\n")
  print(MannKendall(x))
  cat("Kw:\n")
  g <- cut(seq_along(x), breaks=5, labels=FALSE)
  print(kruskal.test(x ~ factor(g)))
  cat("\n\n")
}
print(acf_(y_random, m=40))
print(pacf_(y_random, m=40))
n1 <- sim(500, c(-0.3, 0.7), c(-0.1, -0.1))
n2 <- sim(500, c(0.4, -0.2), c(0.5, 0.3))
n3 <- sim(500, c(-0.6, 0.1), c(-0.4, 0.2))
n4 <- sim(500, c(1, -0.5), c(0.2, -0.7))
n5 <- sim(500, c(-0.9, 0.9), c(0.1, -0.3))
n6 <- sim(500, c(-0.1, 0.1), c(-1.5, 0.9))
l <- list(n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6)
for(i in names(l)) {
  series <- l[[i]]
  plot.ts(series, main=i, ylab=expression(X[t]), col="blue")
  abline(h=0, col="gray")
  print(paste("ACF for", i, ":"))
  acfs = acf_(series, m=40)
  print(acfs)
  barplot(acfs, main=paste("ACF:", i), ylab="ACF", col="steelblue", ylim=c(-1, 1))
  abline(h=0, col="gray")
}
con <- data.frame(p=integer(), q=integer(), AIC=numeric())
for(p in 1:5) {
  for(q in 1:5) {
    for(i in names(a)) {
      y <- a[[i]]
      m = fit(y, p, q)
      con <- rbind(con, data.frame(p=p, q=q, AIC=m$AIC))
    }
  }
}
best <- con[which.min(con$AIC), ]
print(con)
print(best)

err <- list()
err_v <- list()
fl <- list()
y <- y_random
bm = fa(y_random, 2, 2)
horizons <- c(10, 20, 30, 40, 70)
for(h in horizons) {
  frcst <- forecast(bm, level = c(95), h = h)
  fl[["y_random horizon " %>% paste0(h)]] <- frcst
  plot(frcst, main = paste("Forecasted", h, " for y_random"), 
       ylab = expression(X[t]), col = "blue")
  actual <- window(y, start = length(y)-h+1, end = length(y)) 
  frcst_v <- frcst$mean 
  error <- actual - frcst_v
  err[[paste0("y_random horizon ", h)]] <- error
  error_variance <- var(error)
  err_v[[paste0("y_random horizon ", h)]] <- error_variance
  plot(error, type = "o", main = paste("Prediction Errors for y_random Horizon", h), 
       ylab = "Prediction Error", col = "red", pch = 16)
  abline(h = 0, col = "blue", lty = 2)
  barplot(error_variance, main = paste("Variance of Prediction Errors for y_random Horizon", h), 
          ylab = "Error Variance", col = "steelblue")
}
y <- list[["42"]]
y <- y[is.finite(y)]
y <- na.omit(y)
bm = fa(y, 3, 4)
for(h in horizons) {
  frcst <- forecast(bm, level = c(95), h = h)
  fl[["seed_42 horizon " %>% paste0(h)]] <- frcst
  plot(frcst, main = paste("Forecasted", h, "Values for Seed 42"), 
       ylab = expression(X[t]), col = "blue")
  actual <- window(y, start = length(y)-h+1, end = length(y)) 
  frcst_v <- frcst$mean 
  error <- actual - frcst_v
  err[[paste0("seed42 horizon ", h)]] <- error
  error_variance <- var(error)
  err_v[[paste0("seed42 horizon ", h)]] <- error_variance
  plot(error, type = "o", main = paste("Prediction Errors for Seed 42 Horizon", h), 
       ylab = "Prediction Error", col = "red", pch = 16)
  abline(h = 0, col = "blue", lty = 2)
  barplot(error_variance, main = paste("Variance of Prediction Errors for Seed 42 Horizon", h), 
          ylab = "Error Variance", col = "steelblue")
}
y <- list[["101"]]
y <- y[is.finite(y)]
y <- na.omit(y)
bm = fa(y, 2, 2)
for(h in horizons) {
  frcst <- forecast(bm, level = c(95), h = h)
  fl[["seed_101 horizon " %>% paste0(h)]] <- frcst
  plot(frcst, main = paste("Forecasted", h, "Values for Seed 101"), 
       ylab = expression(X[t]), col = "blue")
  actual <- window(y, start = length(y)-h+1, end = length(y)) 
  frcst_v <- frcst$mean 
  error <- actual - frcst_v
  err[[paste0("seed101 horizon ", h)]] <- error
  error_variance <- var(error)
  err_v[[paste0("seed101 horizon ", h)]] <- error_variance
  plot(error, type = "o", main = paste("Prediction Errors for Seed 101 Horizon", h), 
       ylab = "Prediction Error", col = "red", pch = 16)
  abline(h = 0, col = "blue", lty = 2)
  barplot(error_variance, main = paste("Variance of Prediction Errors for Seed 101 Horizon", h), 
          ylab = "Error Variance", col = "steelblue")
}
y <- list[["202"]]
y <- y[is.finite(y)]
y <- na.omit(y)
bm <- fa(y, 4, 4)
for(h in horizons) {
  frcst <- forecast(bm, level = c(95), h = h)
  fl[["seed202 horizon " %>% paste0(h)]] <- frcst
  plot(frcst, main = paste("Forecasted", h, "Values for Seed 202"), 
       ylab = expression(X[t]), col = "blue")
  actual <- window(y, start = length(y)-h+1, end = length(y)) 
  frcst_v <- frcst$mean 
  error <- actual - frcst_v
  err[[paste0("seed202 horizon ", h)]] <- error
  error_variance <- var(error)
  err_v[[paste0("seed202 horizon ", h)]] <- error_variance
  plot(error, type = "o", main = paste("Prediction Errors for Seed 202 Horizon", h), 
       ylab = "Prediction Error", col = "red", pch = 16)
  abline(h = 0, col = "blue", lty = 2)
  barplot(error_variance, main = paste("Variance of Prediction Errors for Seed 202 Horizon", h), 
          ylab = "Error Variance")
}
y <- list[["303"]]
y <- y[is.finite(y)]
y <- na.omit(y)
bm <- fa(y, 2, 5)
for(h in horizons) {
  frcst <- forecast(bm, level = c(95), h = h)
  fl[["seed303 horizon " %>% paste0(h)]] <- frcst
  plot(frcst, main = paste("Forecasted", h, "Values for Seed 303"), 
       ylab = expression(X[t]), col = "blue")
  actual <- window(y, start = length(y)-h+1, end = length(y)) 
  frcst_v <- frcst$mean 
  error <- actual - frcst_v
  err[[paste0("seed303 horizon ", h)]] <- error
  error_variance <- var(error)
  err_v[[paste0("seed303 horizon ", h)]] <- error_variance
  plot(error, type = "o", main = paste("Prediction Errors for Seed 303 Horizon", h), 
       ylab = "Prediction Error", col = "red", pch = 16)
  abline(h = 0, col = "blue", lty = 2)
  barplot(error_variance, main = paste("Variance of Prediction Errors for Seed 303 Horizon", h), 
          ylab = "Error Variance", col = "steelblue")
}
y <- list[["404"]]
y <- y[is.finite(y)]
y <- na.omit(y)
bm <- fa(y, 1, 4)
for(h in horizons) {
  frcst <- forecast(bm, level = c(95), h = h)
  fl[["seed404 horizon " %>% paste0(h)]] <- frcst
  plot(frcst, main = paste("Forecasted", h, "Values for Seed 404"), 
       ylab = expression(X[t]), col = "blue")
  actual <- window(y, start = length(y)-h+1, end = length(y)) 
  frcst_v <- frcst$mean 
  error <- actual - frcst_v
  err[[paste0("seed404 horizon ", h)]] <- error
  error_variance <- var(error)
  err_v[[paste0("seed404 horizon ", h)]] <- error_variance
  plot(error, type = "o", main = paste("Prediction Errors for Seed 404 Horizon", h), 
       ylab = "Prediction Error", col = "red", pch = 16)
  abline(h = 0, col = "blue", lty = 2)
  barplot(error_variance, main = paste("Variance of Prediction Errors for Seed 404 Horizon", h), 
          ylab = "Error Variance", col = "steelblue")
}
cat("forcasts:\n")
print(fl)
cat("Errors for Horizons:\n")
print(err)
cat("Error Variance for Horizons:\n")
print(err_v)
max_error <- max(unlist(err))
cat("Maximum prediction error:", max_error, "\n")

seeds <- c("y_random", "seed42", "seed101", "seed202", "seed303", "seed404")
plot(NULL, xlim = c(min(horizons), max(horizons)), ylim = c(0, 5), 
     xlab = "Horizon", ylab = "Error Variance", main = "Error Variance ")
colors <- c("blue", "red", "green", "purple", "orange", "brown") 
for(i in 1:length(seeds)) {
  seed_name <- seeds[i]
  variances <- sapply(horizons, function(h) err_v[[paste0(seed_name, " horizon ", h)]])
  lines(horizons, variances, type = "b", col = colors[i], pch = 16, lty = 1)
}
legend("topright", legend = seeds, col = colors, pch = 16, lty = 1)
grid()

bm1 <- arima(y_random, order = c(1, 0, 1))  
frcst1 <- forecast(bm1, level = 95, h = 20)
bm2 <- arima(y_random, order = c(1, 0, 2))  
frcst2 <- forecast(bm2, level = 95, h = 20)
bm3 <- arima(y_random, order = c(2, 0, 1)) 
frcst3 <- forecast(bm3, level = 95, h = 20)
bm4 <- arima(sp[["no_seed_trend"]], order = c(2, 0, 2)) 
frcst4 <- forecast(bm3, level = 95, h = 20)
bm5 <- arima(sp[["no_seed_hetero"]], order = c(2, 0, 2)) 
frcst5 <- forecast(bm3, level = 95, h = 20)
plot(frcst1, main = "Model ARIMA(1,0,1)")
plot(frcst2, main = "Model ARIMA(2,0,1)")
plot(frcst3, main = "Model ARIMA(1,0,2)")
plot(frcst4, main = "Model with trend")
plot(frcst5, main = "Model with hetero")




