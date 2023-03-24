pqnumber <- function(sign, p, q, nums) {
  # This function created a pqnumber
  #Args:
  #sign: integer -1 or 1
  #p: integer size 1
  #q: integer size 1
  #nums: integer vector
  #Return:
  # A pqnumber
  if(abs(sign) != 1) {
    stop('Sign should be +/- 1')
  }
  for (i in nums) {
    if (is.numeric(i) == FALSE) {
      stop('nums must be a vector of integers')
    }
  }
  if((p + q + 1) != length(nums)) {
    stop("nums must have length of p + q + 1 integers")
  }
  if(is.numeric(p) == FALSE) {
    stop("p must be an integer")
  }
  if(is.numeric(q) == FALSE) {
    stop("q must be an integer")
  }
  structure(list(sign = as.integer(sign), p = as.integer(p), q = as.integer(q), nums = as.integer(nums)), class = 'pqnumber')
}


is_pqnumber <- function(x) {
  # This function will tell whether or not x is a pqnumber
  #Args:
  #x: any object
  #Return:
  # Single logical value
  any(class(x) == 'pqnumber')
}


print.pqnumber <- function(x, DEC = FALSE) {
  # A way to print a pqnumber
  #Args:
  #x: Singular pqnumber
  #DEC: single logical value
  #Return:
  # printer pqnumber, either in standard form or decimal form
  if(is_pqnumber(x) == FALSE) {
    stop('x must be a pqnumber')
  } 
  if(is.logical(DEC) == FALSE) {
    stop('DEC must be a logical value')
  }
  if(DEC) {
    if(x$sign == -1) {
      sbit <- '-'
    } else {
      sbit <- ''
    }
    decimal <- paste(rev(x$nums[0:x$p]), collapse = '')
    number <- paste(rev(x$nums[(x$p + 1):(x$p+x$q+1)]), collapse = '')
    print_str <- paste(sbit, number, '.', decimal, sep = '')
  } else {
    print_str <- paste0("sign = ", x$sign, '\np = ', x$p, '\nq = ', x$q, '\nnums = ', paste(x$nums, collapse = ' '))
  }
  cat(paste0(print_str, '\n'))
}


as_pqnumber <- function(x, p, q) {
  # Coerces a numeric object into a pqnumber
  #Args:
  #x: Single numeric object
  #p: Single integer
  #q: Single integer
  #Return:
  # Single pqnumber
  UseMethod('as_pqnumber')
}


as_pqnumber.numeric <- function(x, p, q) {
  # Coerces a numeric object into a pqnumber
  #Args:
  #x: Single numeric object
  #p: Single integer
  #q: Single integer
  #Return:
  # Single pqnumber
  if(is.numeric(x) == FALSE) {
    stop('x must be a numeric object')
  }
  if(is.numeric(p) == FALSE) {
    stop("p must be an integer")
  }
  if(is.numeric(q) == FALSE) {
    stop("q must be an integer")
  }
  nums <- (abs(x) * 10^seq(p, -q)) %% 10 %/% 1
  sgn <- if(x == 0) 1 else base::sign(x)
  pqnumber(sgn, p, q, nums)
}


as_numeric <- function(x) {
  # Coerces a pqnumber object into a numeric object
  #Args:
  #x: Single pqnumber 
  #Return:
  # Single numeric object
  UseMethod('as_numeric')
}


as_numeric.pqnumber <- function(x) {
  # Coerces a pqnumber object into a numeric object
  #Args:
  #x: Single pqnumber 
  #Return:
  # Single numeric object
  if(is_pqnumber(x) == FALSE) {
    stop('x must be a pqnumber')
  }
  decimal <- 0
  for(i in 1:length(x$nums)) {
    decimal <- decimal + (x$nums[i] * 10^(i-1))
  }
  options(digits = x$p + x$q + 1)
  exponent <- x$p * -1
  decimal <- decimal * x$sign * (10^exponent)
  decimal
}


abs_gtr <- function(a, b) {
  # Helper function for add, implementation allows subtract() to only use add 
  # and ensures that borrow() is not needed. Takes two pqnumbers and returns
  # whether or not the first inputted pqnumber has a larger magnitude
  #Args:
  #a: Single pqnumber 
  #b: Single pqnumber 
  #Return:
  # Single logical object
  if(is_pqnumber(a) == FALSE) {
    stop('a must be a pqnumber')
  }
  if(is_pqnumber(b) == FALSE) {
    stop('b must be a pqnumber')
  }
  if (a$q > b$q) {
    b <- pqnumber(b$sign, b$p, a$q, c(b$nums, rep(0L, (a$q - b$q))))
  } else if (b$q > a$q) {
    a <- pqnumber(a$sign, a$p, b$q, c(a$nums, rep(0L, (b$q - a$q))))
  }
  for (i in 1:(a$q + 1)) {
    if (rev(a$nums)[i] > rev(b$nums)[i]) {
      return(TRUE)
    } else if (rev(a$nums)[i] < rev(b$nums)[i]) {
      return(FALSE)
    }
  }
  TRUE
}


carry_over <- function(z) {
  # Helper function for add() and multiply(), carries values in a numeric vector post addition to ensure that 
  # all entries are within [0 - 9]
  #Args:
  #z: numeric vector 
  #Return:
  # numeric vector with all entries within [0 - 9]
  if(is.numeric(z) == FALSE) {
    stop('z must be a numeric vector')
  }
  n <- length(z)
  carry <- 0
  for (i in 1:n) {
    zi <- z[i] + carry
    z[i] <- zi %% 10
    carry <- zi %/% 10
  }
  if (carry != 0) {
    z[n+1] <- carry
  }
  z
}


add <- function(x, y) {
  # Adds together 2 pqnumbers
  #Args:
  #x: Single pqnumber 
  #y: Single pqnumber
  #Return:
  # Single pqnumber
  if(is_pqnumber(x) == FALSE) {
    stop('x must be a pqnumber')
  }
  if(is_pqnumber(y) == FALSE) {
    stop('y must be a pqnumber')
  }
  max_p <- max(x$p, y$p)
  max_q <- max(x$q, y$q)
  n <- max_p + max_q + 1
  z <- rep(0L, n)
  if(x$sign == y$sign) {
    x_vals <- x$nums
    y_vals <- y$nums
    sgn <- x$sign
  } else {
    if(abs_gtr(x,y)) {
      x_vals <- x$nums
      y_vals <- -y$nums
      sgn <- x$sign
    } else {
      x_vals <- -x$nums
      y_vals <- y$nums
      sgn <- y$sign
    }
  }
  z[(1 + max_p - x$p):(1 + max_p + x$q)] <- x_vals
  z[(1 + max_p - y$p):(1 + max_p + y$q)] <- z[(1 + max_p - y$p):(1 + max_p + y$q)] + y_vals
  z <- carry_over(z)
  digit_offset <- length(z) - n
  pqnumber(sgn, max_p, max_q + digit_offset, z)
}


subtract <- function(x, y) {
  # Subtracts 2 pqnumbers, uses add function instead of a borrow function
  #Args:
  #x: Single pqnumber 
  #y: Single pqnumber
  #Return:
  # Single pqnumber
  if(is_pqnumber(x) == FALSE) {
    stop('x must be a pqnumber')
  }
  if(is_pqnumber(y) == FALSE) {
    stop('y must be a pqnumber')
  }
  y$sign <- y$sign * -1
  add(x, y)
}



multiply <- function(x, y) {
  # Mulitplies 2 pqnumbers, uses carry_over()
  #Args:
  #x: Single pqnumber 
  #y: Single pqnumber
  #Return:
  # Single pqnumber
  n <- x$p + y$p + x$q + y$q + 1
  z <- rep(0L, n)
  for (r in 1:(1 + y$p + y$q)) {
    x_leftover <- x$p + x$q 
    z[r:(r + x_leftover)] <- z[r:(r + x_leftover)] + (x$nums * y$nums[r])
  }
  z <- carry_over(z)
  digit_offset <- length(z) - n
  sgn <- x$sign * y$sign
  p_new <- x$p + y$p
  q_new <- length(z) - p_new - 1
  pqnumber(sgn, p_new, q_new, z)
}
