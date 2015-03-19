## Put comments here that give an overall description of what your
## functions do

## We have two functions here : 
## makeCacheMatrix - that creates a cacheable matrix
## And cacheSolve -  that conditionally retrieves the inverse of 
## a matrix argument (either from the cache, or by computing it)




## makeCacheMatrix  creates a 'Cacheable' matrix
## it does so by storing the cache in a local environment
## variable, that can be then retrieved in a future call
## it returns a list of functions that hold this environment
makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL 
    }
    get <- function() x
    #stores the inverted matrix in a local environment variable 'm'
    setinverse <- function(x) {m<<-x}
    
    #returns local environment variable m, that has inverted matrix
    getinverse <- function(x) {m}
    
    #debug statements that show the local environment
    #consisting of 4 functions and two variables (m,x)
    print("Created New Environment :")
    print(ls(environment()))
    
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## this function returnes the inverse of the matrix passed as argument
## it tries to first retrieve the inverse from the cache
## if the cache is empty, it computes and caches the inverse
## In either case - it returns the inverted matrix

## TO TEST: call cacheSolve twice with the same input matrix
## the 2nd time, you should see a 'getting cached matrix' message
## on the console
cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #checks if inverse is in the cached environment
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    
    #if not cached,compute the inverse explicitly and cache it
    data <- x$get()
    
    # compute the inverse of m
    m <- solve(data)
    #store inverse in the cache
    x$setinverse(m)
    
    #return inverse
    m
}
