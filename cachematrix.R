
## makeCacheMatrix identifies the location of a cached (stored) inverse of a matrix.  cacheSolve, the first time it is run,
## calculates the inverse of a matrix and stores it at a location identified in makeCacheMatrix. Each subsequent call to 
## cacheSolve produces the matrix inverse without having to recalculate it.


## Input a square matrix, for example Zed <- makeCacheMatrix(matrix_y), and run it.  Input Zed into 
## cacheSolve as cacheSolve(Zed).  The first time cacheSolve is run it calculates the inverse of
## matrix_y.  Each time cacheSolve(Zed) is run thereafter it will call the inverse from cache with the
## message "getting cached data".


## makeCacheMatrix traces the location of four objects: 1) set assigns the input matrix x to y 2) get gets 
## the value of the x matrix 3) setmatrix sets the value of the inverse matrix and 4) getmatrix gets the 
## inverse matrix Minv   

## The input matrix must be square otherwise the function will throw an error

makeCacheMatrix <- function(x = matrix()) {

  Minv <- NULL
  set <- function(y){
    x <<- y                                     #Set x to y
    Minv <<- NULL                               #set matrix inverse to NULL
  }
  get <- function() x                           #Get matrix that will be inverted
  setinv <- function(solve) Minv <<- solve      #Set the value of inverse of matrix x
  getinv <- function() Minv                     #Get the location for the value of the inverse
  list(set = set, get = get,                    #Output the locations of the above to a list
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve computes the inverse of the matrix output from makeCacheMatrix above.  
## If the inverse has already been calculated and not changed it is retrieved from the cache.

cacheSolve <- function(x, ...) {
    
    #input to cacheSolve is the assigned value of makeCacheMatrix, e.g. Zmat <- makeCacheMatrix(x), so
    #input Zmat into cacheSolve, for example, cacheSolve(Zmat)
    
    Minv <- x$getinv()                    #get the matrix inverse from cache
    if(!is.null(Minv)){                   #If it is in the cache return it
      message("getting cached data")
      return(Minv)
    }
    data <- x$get()                       #If not in the cache get the data and calculate the inverse
    Minv <- solve(data,...)
    x$setinv(Minv)                        #Set the value of the inverse in the cache such that on the
                                          #next call to cacheSolve it is not recalculated, note that 
                                          #x$setinv(Minv) puts Minv in makeCacheMatrix 
    Minv                                  #Output the inverse matrix
  }
  