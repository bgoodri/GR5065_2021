# makes it go faster but requires numbers and memoise packages be installed
B <- Vectorize(memoise::memoise(numbers::bell))

Omega <- 0:10
names(Omega) <- as.character(Omega)

log_bell <- function(n) {
  log_n <- log(n)
  log_log_n <- suppressWarnings(log(log_n))
  ratio <- log_log_n / log_n
  out <- rep(NA_real_, length(n))
  small <- n <= 218
  if (any(small)) out[small] <- log(B(n[small]))
  if (all(small)) return(out)
  n_big <- n[!small]
  log_n <- log(n_big)
  log_log_n <- log(log_n)
  ratio <- log_log_n / log_n
  out[!small] <- n_big * (log_n - log_log_n - 1 + ratio + 1 / log_n + 0.5 * ratio ^ 2)
  return(out)
}

log_sum_exp <- function(x) {
  m <- which.max(x)
  return(x[m] + log1p(sum(exp(x[-m] - x[m]))))
}

E <- function(Upsilon = 0) {
  stopifnot(length(Upsilon) == 1)
  if (is.na(Upsilon)) return(NA_real_)
  npU <- 10 + Upsilon
  xpU <- 0:10 + Upsilon
  log_numer <- lchoose(npU, xpU) + log_bell(xpU)
  return(sum(0:10 * exp(log_numer - log_sum_exp(log_numer))))
}

log_Pr <- function(x, n = 10, Upsilon = 0) {
  npU <- n + Upsilon
  xpU <- x + Upsilon
  log_numer <- lchoose(npU, xpU) + log_bell(xpU)
  log_denom <- log_bell(npU + 1)
  if (Upsilon > 0) {
    SEQ <- 0:(Upsilon - 1)
    log_bell_SEQ <- log_bell(SEQ)
    s <- vapply(npU, FUN.VALUE = double(1), FUN = function(n) {
      log_sum_exp(lchoose(n, SEQ) + log_bell_SEQ)
    })
    log_denom <- log_denom + log1p(-exp(s - log_denom))
  }
  return(log_numer - log_denom)
}

Pr <- function(x, n = 10, Upsilon = 0) {
  if (length(n) == 1) return(exp(log_Pr(x, n, Upsilon)))
  mapply(FUN = Pr, x = x, n = n, MoreArgs = list(Upsilon = Upsilon))
}
 
