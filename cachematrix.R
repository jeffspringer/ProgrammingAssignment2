## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y #Assign the input argument to the x object in the parent
                #environment
        inv <<- NULL #Assign the value of NULL to the inv object in the parent
                    #environment.
    }
    get <- function() x #Since the symbol x is not defined within get(), R
                        #retrieves it from the parent environment of makeCacheMatrix().
    setinv <- function(inverse) inv <<- inverse #Since inv is defined in the parent
                                                #environment and we need to access it
                                                #after setinv() completes, the code uses
                                                #the <<- form of the assignment operator
                                                #to assign the input argument to the
                                                #value of inv in the parent environment.
    getinv <- function() inv
    
    #assign each of these functions as an element within a list(), and return
    #it to the parent environment.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
    #Naming the list elements is what allows us to use the $ form of the extract
    #operator to access the functions by name rather than using the [[ form of
    #the extract operator, as in myVector[[2]](), to get the contents of the vector.
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
