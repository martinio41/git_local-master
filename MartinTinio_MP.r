#Martin Tinio MAB2111 Machine Problem


# 1. Define an R function that removes NA values from a vector.
removeNA <- function(x){
  return(x[!is.na(x)])
}

#test
removeNA(c(1, 2, 3, NA, 7, NA, 8))

# 2. Define an R function 
# that computes the factorial of given an integer argument. 
# The output should be a vector of length 1.
calcFactorial <- function(x) {
  res <- 1
  for(i in 1:x){
    res = res * i
  }
  return(res)
}

#test
calcFactorial(1)
calcFactorial(4)



# 4. Define an R function that sorts a given vector in decreasing order. 
# The output should be a vector of the same length. 
# It should accept both numeric or character vectors. 
mySort <- function(x) {
  # Use bubblesort algorithm
  n <- length(x)
  for(j in 1:(n - 1)){
    for(i in 2:(n - j + 1)){
      if(x[i-1] < x[i]){
        temp <- x[i]
        x[i] <- x[i-1]
        x[i-1] <- temp       
      }
    }
  }
  return(x)
}

#test
mySort(c(8,5,9,4,2,1,3))
mySort(c(5,2,6,4,10,1,-3,50,1))
mySort(c("d", "b", "s", "a", "r", "k"))

# 7. Create a function that accepts a vector 
# and integer n and returns nth highest number
nHighestNumber <- function(x, n) {
  sorted <- mySort(x)
  return(sorted[n])
}

#test
nHighestNumber(c(5,8,2,1,3,7,-5), 1)
nHighestNumber(c(5,8,2,1,3,7,-5), 2)
nHighestNumber(c(5,8,2,1,3,7,-5), 3)

# 9. Create a function isPrime(n) that accepts an integer and 
# outputs a Boolean value (TRUE or FALSE) depending 
# whether the integer is a prime number or not.
isPrime <- function(n) {
  if(n == 1) {
    return(FALSE)
  } else if(n == 2) {
    return(TRUE)
  } else {
    for(i in 2:(n-1)){
      if(n %% i == 0){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

#test
isPrime(1)
isPrime(2)
isPrime(3)
isPrime(4)
isPrime(7)
isPrime(282)
isPrime(283)

#test version control