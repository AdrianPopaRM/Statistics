#### C1

# A)
shuffle_randomly = function(n) {
  # Generate n uniformly random values from the interval [0, 1]
  U = runif(n, 0, 1)
  # Initialize the permutation vector with indices from 1 to n
  permutation = 1:n
  
  # Sort the values from U and shuffle the corresponding indices
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

# ************************************************************************* #

# b)
generate_binary_strings = function(n, k) {
  # Generate n binary strings of length k
  bit_strings = list()
  
  for (i in 1:n) {
    string = sample(c(0,1), k, replace = TRUE)
    bit_strings[[i]] = string
  }
  
  return(bit_strings)
}

compare_lexicographically = function(Wi, Wj) {
  # Calculate the minimum length of the two strings
  Lij = min(length(Wi), length(Wj))
  
  # Compare the binary strings bit by bit
  for (h in 1:Lij) {
    if (Wi[[h]] < Wj[[h]])
      return(TRUE) # Wi is strictly less than Wj
    else if (Wi[[h]] > Wj[[h]])
      return(FALSE) # Wi is not strictly less than Wj
  }
  
  # If the strings are equal up to the minimum length, randomly add bits
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

# Generate binary words
n = 6 ; k = 5
words = generate_binary_strings(n, k)
words

# Compare two examples
Wi = words[[4]] ; Wj = words[[6]]

# Result
result = compare_lexicographically(Wi, Wj)
cat("Wi is lexicographically strictly less than Wj:", result, "\n")


# c)
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

# Generate n binary words of k bits
n = 6 ; k = 5
words = generate_binary_strings(n, k)
words

# Display the n words sorted lexicographically
random_quick_sort(words)


# d)
are_equal_words = function(Wi, Wj) {
  # Calculate the minimum length of the two strings
  Lij = min(length(Wi), length(Wj))
  
  # Compare the binary strings bit by bit
  for (i in 1:Lij)
    if (Wi[[i]] != Wj[[i]])
      return(FALSE)
  
  return(TRUE)
}

shuffle_randomly_RQS = function(n, k) {
  # Generate n binary words of k bits
  words = generate_binary_strings(n, k)
  
  # Sort the n words
  sorted_words = random_quick_sort(words)
  
  # Find the indices of the sorted words
  indices = numeric(n)
  for (i in 1:length(sorted_words))
    for (j in 1:length(words))
      if (are_equal_words(sorted_words[[i]], words[[j]])) {
        indices[i] = j
        break
      }
  
  return(indices)
}

print(shuffle_randomly_RQS(6, 5))


#### C2.

# a)
max_cut_random_algorithm = function(n) {
  # Initialize a bipartite graph with 2n nodes and n*n edges
  graph = matrix(0, nrow = n * 2, ncol = n * 2)
  
  for (i in 1:(n * 2))
    for (j in 1:(n * 2)) {
      # Add edges between the first half of the nodes and the second half
      if (i <= n & j > n) 
        graph[i, j] = 1
      # Add edges between the second half of the nodes and the first half
      else if (i > n & j <= n)
        graph[i, j] = 1
    }
  
  # Randomly choose n nodes for A and the rest of n nodes form B
  A = sample(1:(n * 2), n)
  B = setdiff(1:(n * 2), A)
  
  # Determine the edges in the cut
  cut_edges = matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) 
    for (j in 1:n) {
      # Obtain the edges in the cut
      if (graph[A[i], B[j]] == 1)
        cut_edges[i, j] = 1
    }
      
  cut_cardinality = sum(cut_edges)  # Calculate the cardinality of the cut
  
  return(list(A = A, B = B, cut_edges = cut_edges, cut_cardinality = cut_cardinality))
}

# b)
# To increase the chances of finding a maximum cardinality cut,
# we can run the algorithm multiple times and choose the cut with the maximum cardinality
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
