#### D1)

z_confidence_interval = function(file_name, confidence_level) {
  data = read.table(file_name, header = TRUE)
  
  n = length(data$probabilities)
  sample_mean = mean(data$probabilities)
  alpha = 1 - confidence_level
  sigma = sqrt(92.16)
  
  critical_z = qnorm(1 - alpha / 2, 0, 1)
  
  lower_bound = sample_mean - critical_z * sigma / sqrt(n)
  upper_bound = sample_mean + critical_z * sigma / sqrt(n)
  
  return(c(lower_bound, upper_bound))
}

cat("95% Confidence Interval:", z_confidence_interval("probabilities.txt", 0.95), "\n")
cat("99% Confidence Interval:", z_confidence_interval("probabilities.txt", 0.99), "\n")

##### D2)

t_conf_interval = function(file_name, confidence_level) {
  data = read.table(file_name, header = TRUE)
  
  n = length(data$statistics)
  sample_mean = mean(data$statistics)
  alpha = 1 - confidence_level
  sigma = sd(data$statistics)
  
  se = sigma / sqrt(n)
  
  critical_t = qt(1 - alpha / 2, n - 1)
  
  lower_bound = sample_mean - critical_t * se
  upper_bound = sample_mean + critical_t * se
  
  return(c(lower_bound, upper_bound))
}

cat("95% Confidence Interval:", t_conf_interval("statistics.txt", 0.95), "\n")
cat("99% Confidence Interval:", t_conf_interval("statistics.txt", 0.99), "\n")

##### D3)

test_proportion = function(n, alpha, successes, p0) {
  p_prim = successes / n
  z_score = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  critical_z = qnorm(1 - alpha, 0, 1)
  
  cat("Test Statistic", alpha * 100, "%:", "z_score:", z_score, "| critical_z:", critical_z)
}

n = 100
successes = 86 # At most 14 cannot solve the assignments
p0 = 0.85

cat(test_proportion(n, 0.01, successes, p0), "\n")
cat(test_proportion(n, 0.05, successes, p0), "\n")
