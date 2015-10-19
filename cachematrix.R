#this function is to create the 'special matrix' that can also cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  setit <- function(y) {
    x <<- y
    invr <<- NULL
  }
  getit <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(setit=setit, getit=getit, setinverse=setinverse, getinverse=getinverse)
}


# this function calculcates the inveres from the above function makeCacheMatrix. 
#If the inverse has been claculated then it will bring back the inverse

cacheSolve <- function(x, ...) {
        
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("Getting cached data")
    return(invr)
  }
  myData <- x$getit()
  invr <- solve(myData)
  x$setinverse(invr)
  invr
}
