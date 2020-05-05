# ==============================================================
# File:     Factorial_code.R
# Author:   Daniel DeWaters
# Date:     5/4/2020
# Purpose:  Impliment different methods for computing factorials 
# ==============================================================

library(purrr)
library(microbenchmark)

# Compute factorials using a loop
factorial_loop <- function(n){
  if(n == 0){return(1)}
  else{
    f <- 1
    for(i in 1:n){f <- f * i}
    return(f)
  }
}

# Compute factorials using the reduce() function
factorial_reduce <- function(n){
  if(n == 0){return(1)}
  else{reduce(1:n, function(x,y){x * y})}
}

# Compute factorials using recursion
factorial_func <- function(n){
  if(n == 0 | n==1){return(1)}
  else{return(n * factorial_func(n-1))}
}


# Compute factorials using memoization
my_n <- 10
mem <- c(1, rep(NA, times=my_n-1))

factorial_mem <- function(n){
  if(n==0 | n==1){return(1)}
  else{
    if(!is.na(mem[n])){return(mem[n])}
    else{
      new_n <- n * factorial_mem(n-1)
      mem[n] <<- new_n
      return(new_n)
    }
  }
}

# Function for running microbenchmark on all factorial methods
run_microbenchmark <- function(n){
  summary(microbenchmark(factorial_loop(n), 
                         factorial_reduce(n),
                         factorial_func(n),
                         factorial_mem(n)))
}

as.data.frame(run_microbenchmark(10))