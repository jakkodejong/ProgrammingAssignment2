## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that stores a matrix 'x' as a global variable in the global namespace. 
## Furthermore, it creates a global variable 'inv', with initial value NULL, to store the inverse of matrix x in the global namespace.

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inv_matrix) inv <<- inv_matrix
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve takes the function list created by makeCacheMatrix, gets the stored matrix, and finds out if an inverse matrix was already cached.
## If so, it returns this cached inverse matrix. If not, it calculates the inverse and stores the result by calling x$setInv().                

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached inverse of matrix x.")
                return(inv)
        }
        message("calculating inverse of matrix x.")
        data <- x$get()
        inv <- solve(x$get())
        x$setInv(inv)
        inv
}
