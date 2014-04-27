## Put comments here that give an overall description of what your
## functions do

## Creates an object with four function methods that construct and retrieve the matrix
## itself and its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- matrix() #create a 1x1 matrix containing a value to hold the inverse
     set <- function(y = matrix()){ # create a function, set, to eventually the input as its value
       x <<- y
       inv <<- matrix() # re-initialize the inverse holder to a "blank"
     }
     get <- function() x # output whatever matrix we stored originally
     setinv <- function(inverse) inv <<- inverse # if we calculated some inverse set inv 
     # equal to it
     getinv <- function() inv # output the value of whatever inverse we've computed
     list(set = set, #create a list with the different functions
          get = get,
          setinv = setinv,
          getinv = getinv)
}


## Calculates inverse of cacheMatrix object if inv not initialized, otherwise
## outputs cached object

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinv() #output the inverse that we stored
  if(sum(dim(inv))== 2 && all(is.na(matrix()))){ #if the matrix has exactly dimesion one
    #and is composed of a single NA
    operand <- x$get() #get the operand, our matrix, out of the list
    inv <- solve(operand) # find its inverse
    x$setinv(inv) #set its inverse
    inv #print that inverse
  } 
  else{
    message("Returning cached inverse") #otherwise print message
    return(inv) # and return the previously calculated, cached inverse
  }
  
    
}
