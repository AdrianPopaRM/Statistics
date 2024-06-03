#A1 a)

library(stats)

calculate_probabilities <- function(lambda,p,n, m, k) {
  poisson_probs <- ppois(k:m, lambda)
  geometric_probs <- pgeom(k:m, p)
  binomial_probs <- pbinom(k:m, n, p)
  
  return(list(poisson_probs = poisson_probs, geometric_probs = geometric_probs, binomial_probs = binomial_probs))
}

#A1 b)
plot_pmf <- function(lambda,p,n,m, k) {
  x <- k:m
  
  plot(x, dpois(x, lambda), type = "h", col = "red", main = "Probability Mass Function",xlab = "x",ylab = "Probability")
  lines(x, dgeom(x, p), type = "h", col = "green")
  lines(x, dbinom(x, n, p), type = "h", col = "blue")
  
  legend("topright", legend = c("Poisson", "Geometric", "Binomial"), fill = c("red", "green", "blue"))
}

#A1 c)

find_k <- function(lambda) {
   k <- 0
  while( ppois(k , lambda )<= 1 - 10^-6) {
    k <- k + 1
  }
   
  return(k)
}

lambda <- 10
p <- 0.5
n <- 20
m <- 15
k <- 5

probabilities <- calculate_probabilities(lambda, p, n, m, k)
print(probabilities)

plot_pmf(lambda, p, n, m, k)

smallest_k <- find_k(lambda)

print(paste("Cea mai mica valoare pentru care P(Y < k) <= 1 - 10^-6 este: ", smallest_k))


# A2 a)

calculate_complement <- function(p) {
  p_complement <- 1 - p
  return(p_complement)
}

# A2 b)

calculate_expected_values <- function(f, n, p) {
  expected_values <- f / n * p
  return(expected_values)
}

p <- 0.7
f <- 10
n <- 20
p_i <- 0.5

p_complement <- calculate_complement(p)
print(paste("The probability of the complement of the event is: ", p_complement))

expected_values <- calculate_expected_values(f, n, p_i)


print(paste("The expected values are: ", expected_values))

