get_sqrt <- function(a, tol = 10^-8, iter_max = 1000, verbose = FALSE) {
  # Takes a nonnegative number a and returns its square root.
  #Args:
  #a: Single nonnegative numeric
  #tol: Single numeric, manual error for when to return value 
  #iter_max: Single integer, max number of iterations before stopping in case of divergence
  #verbose: Logical value that determines whether or not intermediate steps should be printed
  #Return:
  #Singular nonnegative numeric (square root of a)
  if(!is.numeric(a)) {
    stop('a must be a numeric input')
  }
  if(a < 0) {
    stop('a must be nonnegative')
  }
  if(!is.numeric(tol)) {
    stop('tol must be a numeric input')
  }
  if(!is.numeric(iter_max)) {
    stop('iterations must be a numeric input')
  }
  if(!is.logical(verbose)) {
    stop('verbose must be a logical input')
  }
  f <- function(x){
    x*x - a
  }
  f_prime <- function(x){
    2*x
  }
  x <- a
  its <- 0
  while((abs(f(x)) > tol) & (its < iter_max)){
    x <- x - f(x) / f_prime(x)
    its <- its + 1
    if(verbose){
      print(x)
    }
  }
  x
}


get_abroot <- function(a, root, tol = 10^-8, iter_max = 1000, verbose = FALSE) {
  # Takes a nonnegative number a and returns its root with respect to 'root' integer
  #Args:
  #a: Single nonnegative numeric
  #root: Single positive integer
  #tol: Single numeric, manual error for when to return value 
  #iter_max: Single integer, max number of iterations before stopping in case of divergence
  #verbose: Logical value that determines whether or not intermediate steps should be printed
  #Return:
  #Singular nonnegative numeric (specified root of a)
  if(!is.numeric(a)) {
    stop('a must be a numeric input')
  }
  if(!(root%%1 == 0) & !(root < 0)) {
    stop('root must be a positive integer')
  }
  if(a < 0) {
    stop('a must be nonnegative')
  }
  if(!is.numeric(tol)) {
    stop('tol must be a numeric input')
  }
  if(!is.numeric(iter_max)) {
    stop('iterations must be a numeric input')
  }
  if(!is.logical(verbose)) {
    stop('verbose must be a logical input')
  }
  f <- function(x){
    x^root - a
  }
  f_prime <- function(x){
    root*x^(root-1)
  }
  x <- a
  its <- 0
  while((abs(f(x)) > tol) & (its < iter_max)){
    x <- x - f(x) / f_prime(x)
    its <- its + 1
    if(verbose){
      print(x)
    }
  }
  x
}


get_min <- function(f, x, n, a, tol = 10^-8, iter_max = 1000) {
  # Takes a expression f, initial value x, and single numbers n and a and returns
  # the minimum of the expression f 
  #Args:
  #f: single expression
  #x: Single numeric (initial value)
  #a: Single numeric (extra variable for expression f)
  #n Single numeric (extra variable for expression f)
  #tol: Single numeric, manual error for when to return value 
  #iter_max: Single integer, max number of iterations before stopping in case of divergence
  #Return:
  #Singular nonnegative numeric (minimizer of f)
  if(typeof(f) != 'expression') {
    stop('f must be an expression')
  }
  if(!is.numeric(a)) {
    stop('a must be a numeric input')
  }
  if(!is.numeric(n)) {
    stop('n must be a numeric input')
  }
  if(!is.numeric(x)) {
    stop('x must be a numeric input')
  }
  if(!is.numeric(tol)) {
    stop('tol must be a numeric input')
  }
  if(!is.numeric(iter_max)) {
    stop('iterations must be a numeric input')
  }
  f_expr <- f
  f_prime <- D(f_expr, name = "x")
  f_dub_prime <- D(f_prime, name = "x")
  x <- x
  its <- 0
  while((abs(eval(f_prime)) > tol) & (its < iter_max)){
    x <- x - eval(f_prime)/eval(f_dub_prime)
    its <- its + 1
  }
  x
}