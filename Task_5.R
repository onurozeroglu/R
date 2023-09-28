

# Reference on how decorators can be used in R programming: https://blog.esciencecenter.nl/decorators-in-r-ec84eaeca3e3

# required R packages
library(glue)


#====================================================
# 1st. Example: printing the execution time of a call
#====================================================

# Assuming I have the following function and I want to record the execution time with increasing K:

f <- function(K = 1) {
  r <- c()
  # super inefficient loop
  for (k in 1:K) {
    r <- c(r, rnorm(1)) 
  }
}

# I'll create a decorator function '%time_info%' which will take as input the function 'f' will create the 
# 'start_time' variable before we call the function and the 'end_time' variable after the 'f' function was 
# executed. The 'result' object will be created and will show the 'Time elapsed' in the console

`%time_info%` <- function(f) {
  
  wrapper <- function(...) {
    start_time <- Sys.time()                                         # start time
    result <- f(...)                                                 # run the function and save the result
    end_time <- Sys.time()                                           # end time
    cat(glue::glue("Time elapsed: {end_time - start_time}"), '\n')   # Print the elapsed time
    return(result)                                                   # return the result of the function
  }
  
  return(wrapper)
}

#........................................ 1st. version to call the decorator and receive the results
# small value for K
K = 10
short_time_deco <- `%time_info%`(f)(K)

# big value for K
K = 10^4
long_time_deco <- `%time_info%`(f)(K)

#........................................ 2nd. version to call the decorator and receive the results

# define the decorator
decorator_timer <- `%time_info%`(f)

# small value for K
K = 10
short_time_deco <- decorator_timer(K)

# big value for K
K = 10^4
long_time_deco <- decorator_timer(K)


#====================================================
# 2nd. Example: providing dynamic scoping for a call
#====================================================

# common function used both in 'global' and 'local' scoping
f <- function(y) {
  print(paste(x, " -- ", y))
}

#..................................... using the "global" variable

# definition of the global variable
x <- "global"                       

g <- function() {
  x <- "local"
  f(y = "call from local (static)")
}

g()
# "global  --  call from local (static)"

#..................................... using the "local" variable

`%dynamic_call%` <- function(f) {
  
  wrapper <- function(...) {
    # We have to modify the environment of the function "f" to be the 
    # environment from which it is called (using the "parent.frame()" base
    # R function) and we enable that way the "dynamic scoping", i.e. looking up
    # variables in the calling environment rather than in the enclosing environment 
    # is called dynamic scoping.
    # http://adv-r.had.co.nz/Environments.html [ section: "Calling environments" ]
    environment(f) <- parent.frame()
    result <- f(...)
    return(result)
  }
  return(wrapper)
}

g <- function() {
  x <- "local"
  f <- `%dynamic_call%`(f)(y = "call from local (dynamic)")
  return(f)
}

dynamic_deco <- g()
# "local  --  call from local (dynamic)"

