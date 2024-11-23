makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv) 
  }
  mat <- x$get()
  inv <- solve(mat, ...)  
  x$setInverse(inv) 
  inv 
}


mat <- matrix(c(2, 1, 1, 4), 2, 2)
cacheMat <- makeCacheMatrix(mat)
inv1 <- cacheSolve(cacheMat)
print(inv1)
inv2 <- cacheSolve(cacheMat)
print(inv2)

