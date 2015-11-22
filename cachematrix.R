## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation, and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. The functions
## below are to create a special "matrix" can cache its inverse and compute the inverse of the special "matrix" 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        invMatrix <- NULL
        set <- function(y)
        {
                x <<- y
                invMatrix <<- NULL  
        }
        get <- function() x
        setInvMatrix <- function(inverse) invMatrix <<- inverse
        getInvMatrix <- function() invMatrix
           
        list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)   
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{  
        # Call function getInvMatrix() and assign the value to invMatrix
        invMatrix <- x$getInvMatrix()
        
        # Show message when the inversed matrix has exited already
        if(!is.null(invMatrix))
        {
                message("getting cached inverse matrix")
                return(invMatrix)
        }
        
        # Assign value of function get() to matrix
        matrix <- x$get()
        
        # Compute the inverse of matrix
        invMatrix <- solve(matrix, ...)
        
        x$setInvMatrix(invMatrix)
        invMatrix   
}
