simuleaza_variabila_aleatoare <- function(valori, probabilitati) {
  valoare_aleatoare <- sample(valori, size = 1, prob = probabilitati)
  return(valoare_aleatoare)
}

valori <- c(1, 2, 3, 4)
probabilitati <- c(0.1, 0.3, 0.4, 0.2)

valoare_generata <- simuleaza_variabila_aleatoare(valori, probabilitati)
print(paste("Valoarea aleatoare generată:", valoare_generata))
