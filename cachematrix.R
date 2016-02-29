## Assignment : Week 3 R programming
## Developer : Gaurav Tiwari
## Data : 2016-02-29
## The following is my attempt at solving the assignemnt which was pretty much explanable in the porblem statement itself
## I used roughly the same variables and calculations as shown in the example and substituted mean() with solve()

## MakeCacheMatrix  This function creates a cache of the matrix passed on through the argument in the form of a matrix. It creates a
## list of 4 functions which essentially leads to creation of a cachable inverse matrix

##Usage example
## > a <- matrix(c(1:4), nrow =2,ncol =2)
## > z<- makeCacheMatrix(a)
## > cacheSolve(z)

##For the first time, the inverse will be calculated, the passing the last command again will print the message and display the cached
##matrix

makeCacheMatrix <- function(x = matrix()) {
        ##Initiate the inverse matrix m as null
        m <- NULL
        
        ##Assign y to x and set the inverse matrix to null -- Clearing the cache
        set <-function(y){
                x <<- y
                m <<- NULL
        }
        ##Get is used to get the internal R matrix
        get <- function() x
        
        ## To set cacghe inverse matrix where the matrix m is assigned as the "inverse" for the function(setinverse)
        setinverse <- function(inverse) m <<- inverse
        
        ## Cache invese is calculated here
        getinverse<-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## CacheSolve is used to get the inverse of a matrix
## If the inverse of the matrix has already been computed before ("Cached"), the function will use the cache results
##Otherwise it will use the Solve() function to calculate the inverse of the matrix passed as argument and will store the result in the
## Cache for future computations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse() ##get Cached Inverse
        if(!is.null(m)){
                message("Already computed : Getting cached data to print inverse matrix") ##If Data already exists in m
                return(m) ## Return already computed and cached inverse
        }
        else
        {
                data <-x$get() ##Get the matrix x 
                m<-solve(data) ## Compute Inverse
                x$setinverse(m) ## Set the inverse in the cache for future purposes
                return(m) ## Return the inverse of the matrix
        }   
  }
        
