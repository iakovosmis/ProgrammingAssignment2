##The two functions below are calculating the inverse of a given
## matrix.Every time inverse matrix is calculated is being cached
##to avoid recalculation next time.So if inverse of the given matrix
##exists in cache this value is returned, otherwise it is calculated
#from scratch.

##makeCacheMatrix function creates 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## inverse value.
        
        ##sets the value of given parameter y to matrix x
        ##each time a new object of makeCacheMatrix is created.
        ##Inverse value is being set to null to declare that it must be recalculated 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x ## original value of x matrix
        
        ##called by cacheSolve() and stores value
        ##usign superassignment operator.Superassignment operator is needed because
        ##we need to assign the value in a variable outside of the current scope 
        ##and not create a new object.
        setInverse <- function(inverse) i <<- inverse 
       
        
        getInverse <- function() i ## returns inverse matrix value.Will return null
        ##if it does not exist in cache        
        
        #A list object that is accessed each time makeCacheMatrix() is called,       
        #It contains all the  functions declared in makeCacheMatrix so a calling function(e.g cacheSolve())
        # knows how to access those methods
        list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
        
        
        
        
        
}


##It returns the inverse of the given matrix x
##If the inverse has alredy been calculated(exists in cache)
##returns the cached value otherwise uses solve() to calculate
##inverse and store it in cache.
cacheSolve <- function(x, ...) {
        ##calling getInverse() will return either null 
        ##if inverse matrix does not exist in cache,or
        ##the cached value that has been alredy calculated
        inverse<- x$getInverse()
        if(!is.null(inverse)) { ##if inverse exists in cache return its value
                
                
                message("getting cached data")
                return(inverse)
        }
        
        ##inverse matrix does not exist in cache so
        ##we store the original value of x in a new object called data
        ##and the we calculate inverse of using solve() function.
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse) ##store the calculated inverse in cache
        inverse ##return the inverse of matrix x
}
