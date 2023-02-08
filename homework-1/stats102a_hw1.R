gcd <- function(x,y) {
  # This function take in two integers (x and y) and returns their greatest 
  # common denominator. This is done using the Euclidean Algorithm. Errors will
  # be thrown if the inputs are not integers.
  #Args:
  #x: Singular size 1 integer
  #y: Singular size 1 integer
  #Return:
  #Singular size 1 integer (GCD of x and y)
  if (is.numeric(x) == FALSE) {
    stop('Error: One or more of the arguments are not integers.')
  } else if (is.numeric(y) == FALSE) {
    stop('Error: One or more of the arguments are not integers.')
  }
  if (x%%1 != 0 | y%%1 != 0) {
    stop('Error: One or more of the arguments are not integers.')
  }
  x <- abs(x)
  y <- abs(y)
  dividend <- max(x,y)
  divisor <- min(x,y)
  while (divisor != 0) {
    remainder <- dividend %% divisor
    dividend <- divisor
    divisor <- remainder
  }
  dividend
}


lcm <- function(x) {
  # This function takes in a vector of nonzero integers and returns the least 
  # common multiple of the integers. It does so by utilizing the gcd() function 
  # defined above.
  #Args:
  #x: Integer vector of length between 2 and 100
  #Return:
  #Singular size 1 integer (LCM of x)
  n <- length(x)
  if (n < 2 | n > 100) {
    stop('Error: Value not between 2 and 100')
  }
  if (is.numeric(x) == FALSE) {
    stop('Error: Vector must be numeric.')
  }
  if (0 %in% x) {
    stop('Error: One or more entries contain zero')
  }
  if (x[1]%%1 != 0) {
    stop('Error: Values must be integers')
  }
  lcm1 <- abs(x[1])
  for (i in 2:n) {
    if (x[i]%%1 != 0) {
      stop('Error: Values must be integers')
    }
    lcm2 <- abs(x[i]) * lcm1 / gcd(x[i],lcm1)
    lcm1 <- lcm2
  }
  lcm2
}


check_prime <- function(x) {
  # This function takes in a nonzero integer and returns whether or not it is a 
  # prime number through a logical value. This is a helper function used in the 
  # function is_prime().
  #Args:
  #x: Singular non-negative integer
  #Return:
  #Singular logical value
  if (x <= 1) {
    stop('Error: Invalid argument, must be nonnegative integer')
  }
  if (is.numeric(x) == FALSE) {
    stop('Error: Invalid argument, must be nonnegative integer')
  }
  y <- 2
  while (y < x) {
    if (x%%y == 0 | x%%1 != 0) {
      return(FALSE)
    }
    y = y + 1
  }
  return(TRUE)
}


is_prime <- function(values) {
  # This is a function which takes a vector of nonzero integers x and returns a 
  # logical vector object. This vector should give a TRUE whenever the 
  # corresponding x entry is a prime number and FALSE if not. This function 
  # utilizes the helper function check_prime() which returns a logical value 
  # depending on whether or not a singular non-negative integer is prime. 
  # This function itself is also a helper function for get_factors()
  #Args:
  #x: Vector of non-negative integers (length greater than 1)
  #Return:
  #Logical vector of the same length of x (TRUE if prime and FALSE if not)
  primes <- c()
  for (x in values) {
    if (x <= 0) {
      stop('Values must be nonzero and nonnegative')
      if (is.numeric(x) == FALSE) {
        stop('Error: Invalid argument, must be nonnegative integer')
      }
    } else if (x == 1) {
      primes <- c(primes, FALSE) # appends a FALSE
    } else {
      result <- check_prime(x)
      primes <- c(primes, result)
    }
  }
  primes
}


get_factors <- function(x) {
  # This is a function which takes a nonzero integer x and returns a list object. 
  # This list object should contain an entry with a vector of the unique prime 
  # factors of x and another with their corresponding exponents. This function 
  # utilizes the helper function is_prime() which returns a logical vector 
  # depending on whether or not the elements in x are prime.
  #Args:
  #x: Singular non-negative integer
  #Return:
  #List object of length 2 (prime factors and their exponents)
  if (x <= 1 | x%%1 != 0) {
    stop('Error: invalid argument, x must be a positive integer')
    if (is.numeric(x) == FALSE) {
      stop('Error: Invalid argument, must be nonnegative integer')
    }
  } else {
    primes <- c()
    exponents <- c()
    y <- 2
    while (x > 1) {
      if (is_prime(y)) {
        if (x%%y == 0) {
          primes <- c(primes,y)
          count = 0
          while (x%%y == 0) {
            x <- x/y
            count = count + 1
          }
          exponents <- c(exponents, count)
        }
      }
      y <- y + 1
    }
  }
  factors <- list(primes = primes, exponents = exponents)
  factors
}
