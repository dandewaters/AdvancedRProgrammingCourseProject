library(purrr)
library(microbenchmark)

factorial_loop <- function(n){
  if(n == 0){return(1)}
  else{
    f <- 1
    for(i in 1:n){f <- f * i}
    return(f)
  }
}

factorial_reduce <- function(n){
  if(n == 0){return(1)}
  else{reduce(1:n, function(x,y){x * y})}
}

factorial_func <- function(n){
  if(n == 0 | n==1){return(1)}
  else{return(n * factorial_func(n-1))}
}



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

run_bench <- function(n){
  summary(microbenchmark(factorial_loop(n), 
                                       factorial_reduce(n),
                                       factorial_func(n),
                                       factorial_mem(n)))
}

as.data.frame(run_bench(10))