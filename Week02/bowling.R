# computes the x-th Fibonacci number without recursion and with vectorization
F <- function(x) {
  stopifnot(is.numeric(x), all(x == as.integer(x)))
  sqrt_5 <- sqrt(5)
  golden_ratio <- 0.5 * (1 + sqrt_5)
  return(round(golden_ratio ^ (x + 1) / sqrt_5))
}

# probability of knocking down x out of n pins
Pr <- function(x, n = 10) return(ifelse(x > n, 0, F(x)) / (-1 + F(n + 2)))

Omega <- 0:10 # 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
names(Omega) <- as.character(Omega)

# probability of knocking down x_1 pins on the first roll and x_2 on the second
joint_Pr <- matrix(0, nrow = length(Omega), ncol = length(Omega))
rownames(joint_Pr) <- colnames(joint_Pr) <- names(Omega)
for (x_1 in Omega) {
  Pr_x_1 <- Pr(x_1, n = 10)
  for (x_2 in 0:(10 - x_1))
    joint_Pr[x_1 + 1, x_2 + 1] <- Pr_x_1 * Pr(x_2, n = 10 - x_1)
}

total <- row(joint_Pr) - 1L + col(joint_Pr) - 1L
