# This function is used to create a cached inverse matrix.
#4 functions allow to modify (set) or get (get) the value of the matrix to be inverted (x) and to 
# recalculate (setinv) or get (getinv) the value of its inverse (inv). 
#When the function to set the value of x (set) is called, then the matrix inv is no longer 
#the correct inverse of x and, therefore, it is set to NULL.

# makeCacheMatrix: return a list of functions:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {


    # inv will store the cached inverse matrix
    inv <- NULL

    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Getter for the matrix
    get <- function() x

    # Setter for the inverse
    setinv <- function(inverse) inv <<- inverse
    # Getter for the inverse
    getinv <- function() inv

    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


# This function returns the inverse of a matrix that was created with the previous function (makeCacheMatrix).
#If the inverse inv of x  has not been calculated yet (is NULL), or the matrix x changed recently (inv is NULL again),
#then it will recalculate the inverse of x. 
#Return the cached value of the inverse (x$getinv).

cacheSolve <- function(x, ...) {
    
	# Return a matrix that is the inverse of 'x'
	inv <- x$getinv()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return inverse
    inv
}

# Example usage:
# > x <- rbind(c(1, -1/4), c(-1/4, 1))         // Create a matrix x
# > mCM<- makeCacheMatrix(x)                   // Create our special matrix
# > mCM$get()                                  // Return the matrix
# > cacheSolve(mCM)                            // No cache in the first run
# > cacheSolve(mCM)                            // Retrieving from the cache in the second run
 