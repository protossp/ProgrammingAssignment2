## This file contains two small functions. They work together to calculate th
## inverse of a Matrix and temporally store it in the memory to be continuously
## use/retrieve without having to calculate it over and over again.
## 

## Function setupcache grabs a matrix "x". Calculates its inverse by using the
##"solve" function, it then stores the matrix "x" into a variable "x" and the 
##inverse of x is stored in a variable "inv_matrix"

setupcache <- function(x = matrix()) { # defines function, one matrix is required as input
 
    inv_matrix <- NULL #initializes inverse matrix storage 
    set <- function(y) { #defines a function to store the matrix, and re-initializes inv_matrix storage
      inv_matrix <<- NULL
    }
    get <- function() x #recalls matrix from memory
    setinv <- function(solve) inv_matrix <<- solve #calculates the inverse matrix using sol
    getinv <- function() inv_matrix #recall inverse matrix from memory
    list(set = set, get = get,  #returns a list with the functions defined in the setupcache function
         setinv = setinv,
         getinv = getinv)
  }



##This function uses setupcache to either obtain the inverse matrix of a input or
## recall the previously calculated inverse matrix

cacheSolve <- function(x, ...) { #defines the duction, input from setupcache is required
        ## Return a matrix that is the inverse of 'x'

    inv_matrix <- x$getinv()   ##gets the value inv_matrix from setupcache and stoes in inv_matrix
    if(!is.null(inv_matrix)) {  ##checks if inv_matrix has already been calculated, or if need to be calculated
      message("getting cached data") ##if already done, it returns the sotre value for inv_matrix
      return(inv_matrix)
    }
    data <- x$get()  ##if inv_matric is null(i.e. not calculated), grabs the matrix from the memory
    inv_matrix <- solve(data, ...) ##calculates the inverse matrix from the new matrix
    x$setinv(inv_matrix) ##stores the value of inverse matrix into memory/cache
    inv_matrix ##returns inver_matrix to be used.
  }
  
  
