##The following pair of functions computes the inverse matrix 
##(if one doesn't already exist) and then caches 
##that inverse to avoid unnecessary repeated computation

#Function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  stored_inverse <- NULL                   #inverse matrix is set to NULL
  set <- function(y) {                     
    x <<- y                                #sets the value of x to y
    stored_inverse <<- NULL                #sets the value of inverse matrix to NULL   }
    get <- function() x                      #gets the value of matrix x 
    setinverse <- function(new_inverse){     #sets the new value of inverse matrix
      stored_inverse <<- new_inverse
    }
    getinverse <- function() stored_inverse  #gets the cached value of inverse matrix
    list(set = set, get = get,               #returns a list of 4 functions
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  #################################################################
  #Function cacheSolve computes the inverse of the special "matrix" returned by 
  #makeCacheMatrix above. If the inverse has already been calculated, 
  #then the cachesolve should retrieve the 
  #inverse from the cache.
  
  cacheSolve <- function(cache, ...) {
    inv_matrix <- cache$getinverse()       #retrieves old cache value
    if(!is.null(inv_matrix)) {             #checks if inverse matrix already exists
      message("Getting cached data...")    #if it does, prints the message
      return(inv_matrix)                   #and the inverse matrix is returned
    }
    #if the inverse matrix doesn't already exists, then prints a message
    message("No previously cached data. Setting new cached value...")
    inv_matrix <- solve(cache$get())       #inverse matrix is computed
    cache$setinverse(inv_matrix)           #sets new cache value
    inv_matrix                             #returns inverse matrix 
  }
  