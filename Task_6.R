
findMinimum <- function(startingPoint, targetFunction) {
  
  optimizer <- optim(par = startingPoint, fn = targetFunction, method = "L-BFGS-B")
  
  
  minimumValue <- optimizer$value
  minimumPoint <- optimizer$par
  
  
  return(list("minimumPoint" = minimumPoint, "minimumValue" = minimumValue))
}


startingPoint <- c(1, 2)


targetFunction <- function(params) {
  x <- params[1]
  y <- params[2]
  
  
  value <- x^2 + y^2
  
  
  if (x < 0 || y < 0) {
    penalty <- 1e6  # Large penalty
    value <- value + penalty
  }
  
  return(value)
}


solution <- findMinimum(startingPoint, targetFunction)

print(solution)


startingPoint <- c(1, 2)


targetFunction <- function(params) {
  x <- params[1]
  y <- params[2]
  
  
  value <- x^2 + y^2
  
  
  if (x < 0 || y < 0) {
    penalty <- 1e6  # Large penalty
    value <- value + penalty
  }
  
  return(value)
}


solution <- findMinimum(startingPoint, targetFunction)

print(solution)
