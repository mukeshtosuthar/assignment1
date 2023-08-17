#Ques 1

odd_int <- seq(from = 1, by = 2, length.out = 1000)
odd_int

odd_integers <- (1:1000)*2-1
odd_integers

#Ques 2
#Function to generate n fibonacci numbers
generate_fibonacci <- function(n){
  fibonacci <- numeric(n)
  
  fibonacci[1:2] <- c(1, 1) #Initilizing the first to fibonacci numbers
  
  for(i in 3:n){
    fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2] #Calculate fibonacci number
  }
  return(fibonacci)
}

# Generate a vector of the first 500 Fibonacci numbers
fibonacci_vector <- generate_fibonacci(500)
fibonacci_vector

#Ques-3


roll_die_and_check_even <- function(n){
  # Simulate rolling a fair six-sided die
  rolls <- sample(1:6, 1)
  
  #check if the roll result is even
  if(rolls %%2 == 0){
    return(1) #even
  } 
  else{
    return(0) #odd
  }
}

#call the funtion to roll the die and check if it's even
result <- roll_die_and_check_even()
if (result == 1) {
  cat("The die roll result is even.\n")
} else {
  cat("The die roll result is odd.\n")
}

# Ques 4

coin_toss_game <- function(){
  # Simulate tossing a fair coin 15 times
  coin_tosses <- sample(c("heads", "tails"), 15, replace = TRUE)
  
  #count no. of heads
  num_heads <- sum(coin_tosses == "heads")
  
  if(num_heads<8){
    return("lose")
    
  }
  else{
    return("win")
  }
}

#call the function and get the result
result <- coin_toss_game()
cat("Outcome:", result)

#Ques 5

mat <- matrix(1, nrow = 5, ncol = 5)
mat

#Ques 6

diagonal_mat <- diag((1:5), nrow = 5, ncol = 5)
diagonal_mat

#Ques 7

die_roll <- function(){
  roll <- sample((1:6),replace = TRUE)
}
result <- die_roll()
result

mat <- matrix(result, nrow = 10, ncol = 10)
mat

#Ques 8

generate_matrix <- function(n, rho){
  if(n<=0){
    stop("n must be a positive integer")
    
  }
  mat_values <- matrix(rho, n ,n)
  diag(mat_values) <- 1
  
  return(mat_values)
}

n <- 5
rho <- 0.3

result_mat <- generate_matrix(n, rho)
result_mat

#Ques 9

generate_matrix <- function(n, rho){
  mat <- matrix(NA , nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      
      mat[i, j] <- rho^abs(i-j)
      
    }
    
  }
  return(mat)
}

n <- 6
rho <- 0.69
result <- generate_matrix(n, rho)
print(result)


#Ques 10

get_odd_columns <- function(input_matrix) {
  odd_columns <- input_matrix[, seq(1, ncol(input_matrix), by = 2)]
  return(odd_columns)
}

# Example input matrix
input_matrix <- matrix(1:12, nrow = 3)

# Get smaller matrix with odd columns
result_matrix <- get_odd_columns(input_matrix)

# Print the result
print(result_matrix)

#Ques 11

array_4d <- array(1, dim = c(10, 4, 6, 5))
array_4d



#########
# Worksheet based

# Ques 1 - Write an R function to calculate the area of a circle of radius 𝑟 for a user-given value of r

calculate_circle_area <- function(radius) {
  if (radius < 0) {
    stop("Radius cannot be negative.")
  }
  
  area <- pi * radius^2
  return(area)
}

# Prompt user for radius
radius <- readline(prompt = "Enter the radius of the circle: ")
radius <- as.numeric(radius)

# Calculate and print the area
circle_area <- calculate_circle_area(radius)
cat("The area of the circle with radius", radius, "is", circle_area, "\n")


## Ques 2 - Write an R function that returns the larger of two inputs 𝑥 and y

larger_of_two <- function(x, y) {
  if (x > y) {
    return(x)
  } else {
    return(y)
  }
}

# Test the function
x <- 5
y <- 8
largest_value <- larger_of_two(x, y)
cat("The larger value between", x, "and", y, "is", largest_value, "\n")



## Ques 3 - Write an R program that saves the output of 1000 rolls of a fair die and returns the number of time
#the output was an even number.


simulate_die_rolls <- function(num_rolls) {
  rolls <- sample(1:6, num_rolls, replace = TRUE)
  return(rolls)
}

# Simulate 1000 die rolls
num_rolls <- 1000
die_rolls <- simulate_die_rolls(num_rolls)

# Count the number of even rolls
num_even_rolls <- sum(die_rolls %% 2 == 0)

# Print the results
cat("Number of even rolls:", num_even_rolls, "\n")
cat("Number of odd rolls:", num_rolls - num_even_rolls, "\n")



### Ques 4 - Draw 1000 random number between [0, 1] and calculate the proportion of numbers between 0.1 and 0.2.



# Generate 1000 random numbers between 0 and 1
num_samples <- 1000
random_numbers <- runif(num_samples)

# Calculate the proportion of numbers between 0.1 and 0.2
proportion_between_0_1_and_0_2 <- mean(random_numbers >= 0.1 & random_numbers <= 0.2)

# Print the result
cat("Proportion of numbers between 0.1 and 0.2:", proportion_between_0_1_and_0_2, "\n")



### Ques 5 - 



### Ques 6 -

experiment <-function(){
  No_of_days <- 0
  halfPills <- 0
  totalPills <- 100
  while(1){
    No_of_days <- No_of_days + 1
    pillDrawn <- rbinom(n=1,size=1,prob=halfPills/totalPills)
    if(pillDrawn==1){
      halfPills <- halfPills - 1
      totalPills <- totalPills - 1
      break
    }
    else{
      halfPills <- halfPills + 1
    }
  }
  return(No_of_days)
}

times_exp_is_repeated <- 1000

sum <- 0

for(i in 1:times_exp_is_repeated){
  sum <- sum + experiment()
}

sum/1000






