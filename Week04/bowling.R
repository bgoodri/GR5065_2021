F <- function(x) {
  stopifnot(is.numeric(x))
  sqrt_5 <- sqrt(5)
  golden_ratio <- (1 + sqrt_5) / 2
  xp1 <- x + 1
  return( (golden_ratio ^ xp1 - cos(xp1 * pi) * golden_ratio ^ (-xp1)) / sqrt_5 )
}

f <- function(x, n = 10) {
  # https://www.wolframalpha.com/input/?i=Integrate%5B%28+%28%281+%2B+Sqrt%5B5%5D%29+%2F+2%29%5E%28x+%2B+1%29+-+Cos%5B%28x+%2B+1%29+*+Pi%5D+*+%28%281+%2B+Sqrt%5B5%5D%29+%2F+2%29%5E%28-x+-+1%29+%29+%2F+Sqrt%5B5%5D%2C+%7Bx%2C+0%2C+n%7D%5D
  sqrt_5 <- sqrt(5)
  sqrt_5p1 <- sqrt_5 + 1
  varphi <- sqrt_5p1 / 2
  log_varphi <- log(varphi) # equals acsch(2)
  n_pi <- n * pi
  constant <- (3 * (-1 + varphi ^ n)) / log_varphi + 
    (sqrt_5 * (-1 + varphi ^ n)) / log_varphi + 
    (2 * (sqrt_5p1 ^ n  * log_varphi - 2 ^ n * log_varphi * cos(n_pi) + 
            2 ^ n * pi * sin(n_pi))) /
    (sqrt_5p1 ^ n * (pi ^ 2 + log_varphi ^ 2))
  constant <- constant / (5 + sqrt_5)
  return( ifelse(x > n | x < 0, NA_real_, F(x) / constant) )
}

Omega <- 0:10
names(Omega) <- Omega

