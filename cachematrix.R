## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix cretates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inverseMatrix <- NULL
      
      get <- function() x 
      setinverse <- function(inverse) inverseMatrix <<- inverse
      getinverse <- function() inverseMatrix
      
      list(get = get, getinverse = getinverse, 
           setinverse = setinverse)
      
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated, then 
## cacheSolve should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverseMatrix <- x$getinverse()
      
      if(!is.null(inverseMatrix)){
            message("getting cached inverse matrix")
            return(inverseMatrix)
      }
      
      matr <- x$get()
      inverseMatrix <- solve(matr)
      x$setinverse(inverseMatrix)
      inverseMatrix     
}


