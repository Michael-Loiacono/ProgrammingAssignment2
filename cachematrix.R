## These two function can be used to cache an inverse computation. You can then
## look it up easily if needed

## makeCacheMatrix is used to create a list which contains a function that
## 1) sets the value of the matrix - set
## 2) gets the value of the matrix - get
## 3) sets the value of the inverse - setinverse
## 4) gets the value of the inverse - getinverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## set allows you to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
   
    ## get allows you to retrieve the matrix
    get <- function() x
    
    ## set inverse allows you to set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## getinverse allows you to retrieve the value of the inverse
    getinverse <- function() inv
    
    ## list is the final list which contains the above four functions
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
  


## cacheSolve takes the functions from the list created by makeCacheMatrix and 
## calculates the inverse using the solve function
## It will first check to see if the inverse has beem calculated already. If it
## has then it will get the inverse from the cache and not compute it again. If
## not, it will use the solve function to find the inverse and set the value of
## the inverse in the cache using the setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## Check to see if cached value exists. If so it will be returned with a
  ## message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## If the cached value does not exists it will be computed here and then set
  ## as the cached value using setinverse. Finally it will be printed for the
  ## user without a message
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}