# function(m) sum(dpois(data, m, log = TRUE))
lambda <- 5.2

log_likelihood <- vector(mode = "integer")

for (i in 1:length(data)) {
  a <- data[i]*log(lambda)
  b <- lambda
  c <- vector(mode = "numeric")
  for (k in 1:data[i]) {
    c[k] <- log(k)
  }
  log_likelihood[i] <- (a - b - sum(c))
}

sum(log_likelihood)
