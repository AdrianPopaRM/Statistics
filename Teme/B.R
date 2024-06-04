estimate_toroidal_volume = function(main_radius, tube_radius, sample_size) {
  # Initialize the number of points inside the torus
  total_points = 0
  
  # Define the limits of the containing cube
  x1_lo = -main_radius - tube_radius; x1_hi = main_radius + tube_radius
  x2_lo = -main_radius - tube_radius; x2_hi = main_radius + tube_radius
  x3_lo = -tube_radius; x3_hi = tube_radius
  
  # Generate random points in the containing cube
  for (i in 1:sample_size) {
    x_coord = runif(1, x1_lo, x1_hi)
    y_coord = runif(1, x2_lo, x2_hi)
    z_coord = runif(1, x3_lo, x3_hi)
    
    if ((z_coord^2 + (sqrt(x_coord^2 + y_coord^2) - main_radius)^2) < tube_radius^2)
      total_points = total_points + 1
  }
  
  # Calculate the volume of the cube
  cube_volume = (x1_hi - x1_lo) * (x2_hi - x2_lo) * (x3_hi - x3_lo)
  
  # Estimate the volume of the torus
  estimated_volume = (total_points / sample_size) * cube_volume
  
  return(estimated_volume)
}

relative_errors = function(main_radius, tube_radius, samples) {
  for (i in 1:length(samples)) {
    exact_volume = 2 * pi^2 * main_radius * tube_radius^2
    estimated_volume = estimate_toroidal_volume(main_radius, tube_radius, samples[i])
    
    relative_error = abs((estimated_volume - exact_volume) / exact_volume) * 100
    
    print(relative_error)
  }
}

relative_errors(10, 3, c(10000, 20000, 50000))

#### B2

estimate_triangle_area = function(a_side, b_side, c_side, d_side, sample_size) {
  # Initialize the number of points inside the triangle
  total_points = 0
  
  # Generate random points in the quadrilateral containing the triangle
  for (i in 1:sample_size) {
    x_coord = runif(1, a_side, b_side)
    y_coord = runif(1, c_side, d_side)
    
    if (y_coord >= 0 && y_coord <= 2*x_coord && y_coord <= 6 - 3*x_coord)
      total_points = total_points + 1
  }
  
  # Estimate the area of the triangle
  estimated_area = (total_points / sample_size) * ((b_side - a_side)  * (d_side - c_side))
  
  print(estimated_area)
}

estimated_area = estimate_triangle_area(0, 4, 0, 8, 20000)


#### B3)

### a)
Monte_Carlo_a = function(N) {
  sum = 0
  
  for (i in 1:N) {
    x_coord = runif(1, -1, 1)
    sum = sum + (2 * x_coord - 1) / (x_coord^2 - x_coord - 6)
  }
  
  return(2*sum/N)
}

Monte_Carlo_a_avg = function(k, N) {
  estimates = 0
  
  for (i in 1:k)
    estimates[i] = Monte_Carlo_a(N)
  
  cat(mean(estimates), "|", "Exact value:", log(3) - log(2))
}

Monte_Carlo_a_avg(30, 50000)

#### b)
Monte_Carlo_b = function(N) {
  sum = 0
  
  for (i in 1:N) {
    x_coord = runif(1, 3, 11)
    sum = sum + (x_coord + 4) / (x_coord - 3)^(1 / 3)
  }
  
  return(8*sum/N)
}

Monte_Carlo_b_avg = function(k, N) {
  estimates = 0
  
  for (i in 1:k)
    estimates[i] = Monte_Carlo_b(N)
  
  cat(mean(estimates), "|", "Exact value: 61.2")
}

Monte_Carlo_b_avg(30, 50000)

#### c)
Monte_Carlo_c = function(N) {
  sum = 0
  
  for (i in 1:N) {
    x_coord = runif(1, 0, 100)
    sum = sum + x_coord * exp(-x_coord^2)
  }
  
  return(100*sum/N)
}

Monte_Carlo_c_avg = function(k, N) {
  estimates = 0
  
  for (i in 1:k)
    estimates[i] = Monte_Carlo_c(N)
  
  cat(mean(estimates), "|", "Exact value: 0.5")
}

Monte_Carlo_c_avg(30, 50000)


#### B4

# a)
simulate_iSocialize_a = function(n, p, q, initial_users, target_users) {
  users = initial_users
  years = 0
  
  while (users < target_users && years < 10000) {
    # Users leaving the platform
    withdrawn_users = rbinom(1, users, q)
    
    # Remaining users
    remaining_users = users - withdrawn_users
    
    # New users
    new_users = rbinom(1, n, p)
    
    # Current total users
    users = remaining_users + new_users
    
    years = years + 1
  }
  
  return(years)
}

# We simulate multiple times to estimate the average years

results = numeric(1000)
for (i in 1:1000)
  results[i] = simulate_iSocialize_a(1000, 0.25, 0.01, 10000, 15000)
average_years = mean(results)
cat("Average years for iSocialize to reach +15000 users:", average_years, "\n")

# b)
simulate_users = function(initial_users, n, p, q, periods) {
  users = initial_users
  full_years = floor(periods)
  fractional_year = periods - full_years
  
  for (i in 1:full_years) {
    # Users leaving the platform
    withdrawn_users = rbinom(1, users, q)
    
    # Remaining users
    remaining_users = users - withdrawn_users
    
    # New users
    new_users = rbinom(1, n, p)
    
    # Current total users
    users = remaining_users + new_users
  }
  
  # Fractional part
  if (fractional_year > 0) {
    # Users leaving the platform
    withdrawn_users = rbinom(1, users, q * fractional_year)
  
    remaining_users = users - withdrawn_users
    new_users = rbinom(1, n, p * fractional_year)
    users = remaining_users + new_users
    }
  return users
  }
   
