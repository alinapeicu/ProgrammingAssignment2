makeCacheMatrix <- function(x = matrix()) { #declare a function that takes a matrix x as parameter
  inv <- NULL # initialize object inv with null
  set <- function(y) { #the object set is a function with a parameter y
    x <<- y # assigned in cache
    inv <<- NULL  # delare in cache inv null
  }
  get <- function() x # the object get is a function that return x
  setinverse <- function(solve) inv <<- solve # decare a function that assigned puts tha value of parametre solve in cache
  getinverse <- function() inv # getinverse is a function thar returns the value of inv from cache
  list(set = set, get = get, # makeCacheMatrix return a list 
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(mat, ...) { # cacheSolve is a function that takes mat as argument; ... are the value of the function mat
    inverse <- mat$getinverse() # inverse get the value of the matrix mat from cache
    if(!is.null(inverse)) { # if the return value of cache is not null 
        message("getting cached data") # a message is printed
        return(inverse) # and the function ends returning the value of the inverse from cache
    }
    data <- mat$get()# in case that there isn't an inverse in cache for the matrix then data gets the value if the matrix
    inverse <- solve(data, ...) # the inverse of the matrixs is calculated
    mat$setinverse(inverse)  # the inverse value is put in cache
    inverse # the functon return the value of the inverse
}
