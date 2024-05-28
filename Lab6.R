#II.6

selection_mean <- function(filename) {
  X <- scan(filename)
  
  m <- mean(X)
  n <- length(X)
  sigma <- 5
  z <- qnorm(0.975)
  
  
  error_margin <- z * (sigma / sqrt(n))
  lower_bound <- m - error_margin
  upper_bound <- m + error_margin
  return(list(mean = m, n = n, lower_bound = lower_bound, upper_bound = upper_bound))
}

result <- selection_mean("history.txt")
print(paste("Sample mea: ", result$mean))
print(paste("Sample size: ", result$n))
print(paste("95% Confidence interval: [", result$lower_bound, ", ", result$upper_bound, "]"))


#III.4


selection_mean <- function(filename) {
  X <- scan(filename)
  m <- mean(X)
  n <- length(X)
  

    sigma <- sd(X) / sqrt(n)
  
  z_95 <- qnorm(0.975)
  lower_95 <- m - z_95 * sigma
  upper_95 <- m + z_95 * sigma
  
  
    z_99 <- qnorm(0.995)
  lower_99 <- m - z_99 * sigma
  upper_99 <- m + z_99 * sigma
  
  return(list(mean = m, n = n, CI_95 = c(lower_95, upper_95), CI_99 = c(lower_99, upper_99)))
}

result <- selection_mean("history.txt")
print(paste("Sample Mean: ", result$mean))
print(paste("Sample Size: ", result$n))
print(paste("95% Confidence Interval: [", result$CI_95[1], ", ", result$CI_95[2], "]"))
print(paste("99% Confidence Interval: [", result$CI_99[1], ", ", result$CI_99[2], "]"))


#IV.2

numar_componente_defecte <- 20
total_componente <- 150
proportie_asteptata <- 0.10
nivel_semnificatie <- 0.05

test_proportie <- prop.test(numar_componente_defecte, total_componente, p = proportie_asteptata, alternative = "greater")
 print(test_proportie)

if (test_proportie$p.value < nivel_semnificatie) {
  cat("Se poate afirma cu un nivel de semnificatie de 5% ca procentul componentelor defecte este mai mare de 10%.\n")
} else {
  cat("Nu se poate afirma cu un nivel de semnificatie de 5% ca procentul componentelor defecte este mai mare de 10%.\n")
}



