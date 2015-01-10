## INSTRUCTION: 
# first build your square matrix
# > x <- makeCacheMatrix(rbind(c(1,2),c(3,4)))
# you'll have 4 functions:
# > names(x)
# [1] "set"        "get"        "setinverse" "getinverse"
# "set" to memorize your matrix, "get" to retrieve, "setinverse" to
# memorize the inverse matrix and "getinverse" to retrieve the inverse
# matrix
# > x$get()
#      [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# you have the inverse matrix with
# > inverse <- cacheSolve(x)
# > inverse
#      [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# and you can check the cache with
# > x$getinverse()
#       [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5

## Cache and retrive your square matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # get - Return the original square matrix
  get <- function(){
    x
  }
  # set the square matrix
  set <- function(m){
    x <<- m
    inverse <<- NULL
  }
  # set the inverse matrix
  setinverse <- function(inv){
    inverse <<- inv
  } 
  # get the inverse matrix
  getinverse <- function(){
    inverse
  }
  
  # return
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Retrieve from cache the inverse matrix or calculate and store it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  # check if the inverse is cached and return it 
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  #else calculate and cache it
  squareMatrix <-x$get()
  inverse <- solve(squareMatrix, ...)
  x$setinverse(inverse)
  
  # return the inverse
  inverse
}
