##makeCacheMatrix creates an R object which is essentially a list of function that
## 1. define a matrix
## 2. get the value of that matrix
## 3. set the value of that matrix's inverse
## 4. get the value of that matrix's inverse

##cacheSolve checks to see whether the inverse of the input matrix has already been calculated,
## and if so simply returns that value using getinverse. If the inverse of that particular matrix
## has not already been calculated then it calculates the inverse using the solve() function.
makeCacheMatrix <- function(x) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

cacheSolve <- function(x,...) {
  inv1 <- x$getinverse()
  if(!is.null(inv1)) {
    return(inv1)
  }
  mat <- x$get()
  inv1 <- solve(mat,...)
  x$setinverse(inv1)
  inv1
  
}
