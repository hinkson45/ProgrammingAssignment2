## makeCacheMatrix takes an invertable matrix as its agrument and returns
## a list object containing functions that will be called by cacheSolve. 
## The purpose of these functions is to cache the inverse matrix so it
## isn't calculated everytime it is needed.

## Takes an invertable matrix and returns a list of functions used by 
## cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
        inv_mtx <- NULL #set inverse matrix to null
        
        set <- function(y) { #set function to reset x
                x <<- y
                inv_mtx <<- NULL
        }
        
        get <- function() x #return x
        
        setinverse <- function(inverse) inv_mtx <<- inverse #set the inverse matrix
        
        getinverse <- function() inv_mtx #retrun the inverse matrix when called
        
        list(set = set, get = get,   #return a list object with callable funcions
             setinverse = setinverse,
             getinverse = getinverse)
}


## Solves and caches inverse matrix for a makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtx_inv <- x$getinverse() #check to see if value is in cache
        
        if(!is.null(mtx_inv)) { 
                message("getting cached data")
                return(mtx_inv) #if already Cached
        }
        
        data <- x$get() #get the matrix if it's not cached
        mtx_inv <- solve(data, ...) #solve for the inverse matrix
        x$setinverse(mtx_inv) #cache the new inverse matrix
        
        mtx_inv #return value          
}

