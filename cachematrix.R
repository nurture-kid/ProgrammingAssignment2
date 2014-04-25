## The program provides mechanisms for caching an inverse of a matrix and using it

## makeCacheMatrix function takes a matrix as a parameter, and caches the matrix and its inverse in memory.
## It also provides functionality for extracting the cached values 

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL

set<-function(y){
  x<<- y
  inv<<- NULL
  
}
get<-function() x

setInv<-function(inverse) inv<<-inverse
getInv<-function() inv

list(set = set, get = get,
     setInv = setInv,
     getInv = getInv)
}


## cacheSolve function takes a matrix of makeCacheMatrix type.
## if the inverse of the matrix already exists for that matrix, it will return the cached value; 
## if the inverse does not exist, it will calculate and return the inverse matrix, and store it in the cache for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data<-x$get()

  inv<-solve(data)
  x$setInv(inv)
  inv
}
