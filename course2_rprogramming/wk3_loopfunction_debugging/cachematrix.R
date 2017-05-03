##
# Below are two functions that are used to create a
# special object that stores a square matrix and caches its inverse.
## 


##
# The first function, `makeCacheMatrix` creates a special "matrix" object
# which is really a list containing 4 functions. 
# Please note that functions are objects as well
# The 4 functions are listed below
# 1.  set the value of the matrix
# 2.  get/return the value of the matrix
# 3.  set(not calcuate) the inverse of the matrix 
# 4.  get/return the inverse of the matrix
##

makeCacheMatrix <- function(x = matrix()) {
    
    #First lets check if a matrix object is passed
    #Then check if the matrix passed is square or not
    #This is not required by assignment instruction, but nice to have it
    if (class(x) != "matrix") {
        stop("Please pass a matrix object to function cacheSolve.")
    }
    else if (nrow(x) != ncol(x)) {
        stop("Please pass a square matrix object function cacheSolve.")
    }
    
    inv <- matrix()
    
    set <- function(y) {
        # x and inv are in the defining environment of this function
        # not the current environment
        x <<- y 
        inv <<- NULL
        
    }
    get <- function() x
    setInv <- function(inverse) {
        inv <<- inverse
    }
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


##
# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinverse`
# function.
##

cacheSolve <- function(x, ...) {
    
    inv <- x$getInv()
    # is.na(matrix()) returns a 1x1 logic matrix instead of length 1 logic vector
    # use all() function to check if the matrix is empty
    # all() returns a logic vector of length 1
    if(!all(is.na(inv))) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
