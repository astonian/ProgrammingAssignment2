## Put comments here that give an overall description of what your
## functions do

## Function return 4 functions set/get/setim/getim as list bound with an 
## input matrix vector x. We will also have input matrix x & inverse matrix im
## bound to it as well. We will have 6 things tied to it - 4 functions + 2 matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y){
      x <<- y
      im <- NULL
    }
    get <- function(){
      x
    }
    setim <- function(solve){
      im <<- solve
    }
    getim <- function(){
      im
    }
    list( set = set, get = get, setim = setim, getim = getim)
}


## Function returns inverse matrix as output for a given input matrix x.
## inverse for the matrix x will be stored when used first time as well.
## if same matrix x is given again, it will be retrieved from cache instead
## of calculating again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getim()
    if(!is.null(im)) {
    #  message("getting cached data")
      return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setim(im)
    im
}

## This function is test function that has 3 matrix a.b & c
## Loops through many times doing Inverse matrix operation that will use cached 
## value.expect this to be quick

testcache <- function(){
  # Initialize 3 matrix
  a <- matrix(NA,3,3); b <- matrix(NA,3,3) ; c <- matrix(NA,2,2)
  # Define 2 matrix
  a[1,1] <- 1;a[1,2] <- 2;a[1,3] <- 3;a[2,1] <- 0;a[2,2] <- 4;a[2,3] <- 5;a[3,1] <- 1;a[3,2] <- 0;a[3,3] <- 6
  b[1,1] <- 7;b[1,2] <- 2; b[1,3] <- 1 ; b[2,1] <- 0; b[2,2] <- 3; b[2,3] <- -1; b[3,1] <- -3;b[3,2] <- 4; b[3,3] <- -2
  # Find another matrix C
  c <- a %*% b
  # cache the matrix, inverse matrix
  ma <- makeCacheMatrix(a);mb <- makeCacheMatrix(b);mc <- makeCacheMatrix(c)

  #Loop through 10,000 times Inverse matrix operation solve using cache
  for (i in 1:10000) {
    a %*% cacheSolve(ma)
    b %*% cacheSolve(mb)
    c * cacheSolve(mc)
    cacheSolve(ma) %*% cacheSolve(mb)
    cacheSolve(ma) %*% cacheSolve(mc)
    cacheSolve(mc) %*% cacheSolve(mb)
  }
  
}

## This function is test function that has 3 matrix a.b & c
## Loops through many times doing Inverse matrix operation.
## expect this to be taking time

testnocache <- function(){
  a <- matrix(NA,3,3); b <- matrix(NA,3,3) ; c <- matrix(NA,2,2)
  a[1,1] <- 1;a[1,2] <- 2;a[1,3] <- 3;a[2,1] <- 0;a[2,2] <- 4;a[2,3] <- 5;a[3,1] <- 1;a[3,2] <- 0;a[3,3] <- 6
  b[1,1] <- 7;b[1,2] <- 2; b[1,3] <- 1 ; b[2,1] <- 0; b[2,2] <- 3; b[2,3] <- -1; b[3,1] <- -3;b[3,2] <- 4; b[3,3] <- -2
  c <- a %*% b
 
  for ( i in 1:10000) {
    a %*% solve(a)
    b %*% solve(b)
    c %*% (solve(c))
    solve(a) %*% solve(b)
    solve(a) %*% solve(c)
    solve(c) %*% solve(b)
  }
}

# Final test funnction to run to know the speed of operation.
testnow <- function(){
  print("not cached run time")
  print(system.time(testnocache()))
  print("cached run time")
  print(system.time(testcache()))
}