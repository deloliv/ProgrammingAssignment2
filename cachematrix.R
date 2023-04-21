## This program creates 2 functions to cach the inverse of a matrix
 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## - function argument 'x' : corresponds to the input matix, it is by default an empty matrix
makeCacheMatrix <- function(x = matrix()) {
   ## initialization of mi as a matrix to be used later
   mi <- matrix()
   ## define set() function
   set <- function(y) {
      x <<- y
      mi <<- matrix()
   }
   ## retrieve x from the parent environment of makeCacheMatrix (getter)
   get <- function() x
   ## assign mi in the parent environment (setter)
   setinv <- function(inv) mi <<- inv
   ## retrieve mi from parent environment
   getinv <- function() mi
   
   ## each function created above are assigned as element of a list
   ## each element is named
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## - function argument 'x' : corresponds to the input matix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinv()
        ## check if minv is a valid matrix
        if(!all(is.na(minv))){
            message("getting cached data")
            return(minv)
        }
        
        ## if matrix inverse does not exist
        ## retrieve x from function argument
        matrice <- x$get()
        ## assign the matrix inverse to minv
        minv <- solve(matrice)
        ## set the matrix inverse in the input object
        x$setinv(minv)
        ## print the matrix inverse
        minv
    } 
