#### C1

#### a)
shuffle_randomly = function(n) {
  U = runif(n, 0, 1)
  
  permutation = 1:n
  
  for (i in 2:n) {
    key = U[i]
    key_index = permutation[i]
    j = i - 1
    
    while (j >= 1 && U[j] > key) {
      U[j + 1] = U[j]
      permutation[j + 1] = permutation[j]
      j = j - 1
    }
    
    U[j + 1] = key
    permutation[j + 1] = key_index
  }
  
  cat("The random permutation looks like this:", permutation)
}

shuffle_randomly(10)


#### b)
generate_binary_strings = function(n, k) {

  bit_strings = list()
  
  for (i in 1:n) {
    string = sample(c(0,1), k, replace = TRUE)
    bit_strings[[i]] = string
  }
  
  return(bit_strings)
}

compare_lexicographically = function(Wi, Wj) {
  Lij = min(length(Wi), length(Wj))
  

  for (h in 1:Lij) {
    if (Wi[[h]] < Wj[[h]])
      return(TRUE) 
    else if (Wi[[h]] > Wj[[h]])
      return(FALSE) 
  }
  

  while (TRUE) {
    if (length(Wi) < length(Wj))
      Wi = c(Wi, sample(c(0, 1), 1))
    else if (length(Wj) < length(Wi))
      Wj = c(Wj, sample(c(0, 1), 1))
    else {
      Wi = c(Wi, sample(c(0, 1), 1))
      Wj = c(Wj, sample(c(0, 1), 1))
    }
    
    if (Wi[[length(Wi)]] < Wj[[length(Wj)]])
      return(TRUE)
    else if (Wi[[length(Wi)]] > Wj[[length(Wj)]])
      return(FALSE)
  }
}

n = 6 ; k = 5
words = generate_binary_strings(n, k)
words

Wi = words[[4]] ; Wj = words[[6]]

result = compare_lexicographically(Wi, Wj)
cat("Wi is lexicographically strictly less than Wj:", result, "\n")


#### c)
random_quick_sort = function(S) {
  if (length(S) <= 1)
    return(S)
  
  pivot_index = sample(1:length(S), 1)
  pivot = S[[pivot_index]]
  
  left = list() ; right = list()
  
  for (i in 1:length(S))
    if (i != pivot_index) {
      if (compare_lexicographically(S[[i]], pivot) == TRUE)
        left = c(left, list(S[[i]]))
      else
        right = c(right, list(S[[i]]))
    }
  
  sorted_left = random_quick_sort(left)
  sorted_right = random_quick_sort(right)
  
  return(c(sorted_left, list(pivot), sorted_right))
}

n = 6 ; k = 5
words = generate_binary_strings(n, k)

words


random_quick_sort(words)


#### d)
equal_words = function(Wi, Wj) {

  Lij = min(length(Wi), length(Wj))  

  for (i in 1:Lij)
    if (Wi[[i]] != Wj[[i]])
      return(FALSE)
  
  return(TRUE)
}

shuffle_randomly_RQS = function(n, k) {

  words = generate_binary_strings(n, k)
  
  sorted_words = random_quick_sort(words)
  
  indices = numeric(n)
  for (i in 1:length(sorted_words))
    for (j in 1:length(words))
      if (equal_words(sorted_words[[i]], words[[j]])) {
        indices[i] = j
        break
      }
  
  return(indices)
}

print(shuffle_randomly_RQS(6, 5))


#### C2

#### a)
max_cut_random_algorithm = function(n) {

  graph = matrix(0, nrow = n * 2, ncol = n * 2)
  
  for (i in 1:(n * 2))
    for (j in 1:(n * 2)) {

      if (i <= n & j > n) 
        graph[i, j] = 1

      
      else if (i > n & j <= n)
        graph[i, j] = 1
    }
  
  A = sample(1:(n * 2), n)
  B = setdiff(1:(n * 2), A)
  
  cut_edges = matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) 
    for (j in 1:n) {
      # Obtain the edges in the cut
      if (graph[A[i], B[j]] == 1)
        cut_edges[i, j] = 1
    }
      
  cut_cardinality = sum(cut_edges)
  
  return(list(A = A, B = B, cut_edges = cut_edges, cut_cardinality = cut_cardinality))
}

#### b)

increase_max_cut_chance = function(n, num_trials) {
  max_cut = NULL
  max_cut_cardinality = 0
  for (i in 1:num_trials) {
    result = max_cut_random_algorithm(n)
    if (result$cut_cardinality > max_cut_cardinality) {
      max_cut = result
      max_cut_cardinality = result$cut_cardinality
      }
    }
  return(max_cut)
  }

