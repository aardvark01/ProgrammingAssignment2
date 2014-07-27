## These functions are used to store a matrix and its inverse Matrix.

## makeCacheMatrix is used to cache a matrix and the corresponding inverse matrix.
## set is called by default when you first initialize the makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ##initialize the inverse matrix to NULL
        
        ## reset the matrix to a new matrix
        set <- function(y) {
                x <<- y  ## Cache the new matrix
                m <<- NULL  ##reset and cache the inverse matrix to null
        }
        
        ## returns the starting matrix
        get <- function() x
        
        ## Cache the inverse matrix
        setinverse <- function(matrix) m <<- matrix
        
        ## get the cached inverse matrix 
        getinverse <- function() m
        
        ##names list for calling functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes in a makeCacheMatrix variable and determines the inverse of
## the matrix that is currently loaded into the function.

## If the matrix hasn't chanced, the function will just return the cached value.
## If the matrix is not solvable, the fuction will return 0;

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## if this is already solved, don't resolve.
        i <- x$getinverse()
        if(is.null(i)){
        
                ## make sure you can solve for the inverse of the matrix.
                d <- det(x$get())
                if(d==0){
                        ## Can't solve the matrix
                        message("Cannot Find Inverse of Matrix")
                        return(d)  ## return 0
                }

                i <-solve(x$get())  ## determine the inverse of the matrix
                x$setinverse(i)  ## cache the invese matrix
        }
        i
}


## Test funtion to verify the operation of these functions
testSolve <- function(){
        amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
        print("initial the matrix")
        
        print("Return the matrix")
        print(amatrix$get()) 
        
        print("Computes, caches, and returns    matrix inverse")
        print(cacheSolve(amatrix))
        
        print("Return matrix inverse")
        print(amatrix$getinverse())
        
        print("Returns cached matrix inverse using previously computed matrix inverse")
        print(cacheSolve(amatrix))
        
        print("Modified the matrix")
        amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))
        
        print("Computes, caches, and returns matrix inverse")
        print(cacheSolve(amatrix)) 
        
        print("Return the matrix")
        print(amatrix$get()) 
        
        print("Return matrix inverse")
        print(amatrix$getinverse()) 
        
        print("Modified the matrix")
        amatrix$set(matrix(c(3,5,1,6,2,2,3,1,1), nrow=3, ncol=3)) # Modify existing matrix
        
        print("Attempt to compute the matrix inverse should fails.")
        print(cacheSolve(amatrix))
}
