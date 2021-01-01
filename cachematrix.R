## Put comments here that give an overall description of what your
## functions do

#The first function's objective is to create an environment that contains 
#a matrix to be iverted and the necessary functions to store all its contents
#in a parent environment so it can be reused as needed.
#The second function creates an inverse matrix from the original matrix or 
#retrieves its result if the calculation has already been done and stored in 
#the parent environment. 

## Write a short comment describing this function
#This function stores functions and data objects in a parent environment: 
#two empty data objects (one for the matrix and one for its unversed matrix) 
#and four functions. First -function set() assigns the input argument to 
#the x object in the parent environment and assigns NULL's value to 
#the minv object in the parent environment. The Second function gets the 
#matrix in x. The Third function sets the inverse of the matrix in minv 
#in the parent environment. The fourth function is the getter for the 
#inversed matrix to be retrieved from the parent environment. All functions 
#are placed in a named list that can be later accessed from the parent 
#environment.

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y){
    x <<- y
    im <<- NULL
  }
  
  get <- function() x
  setim <- function(solve) im <<- solve
  getim <- function() im
  list(set = set, get = get,
       setim = setim,
       getim = getim)
}


## Write a short comment describing this function
#This function uses the environment created in the first function. 
#It takes the matrix stored in the first function and, if it hasn't been 
#inverted, applies the "solve() method." If it has been already inverted, 
#prints the message "getting cache data" and retrieves the inverted matrix 
#from the parent environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getim()
  if(!is.null(im)) {
    message("getting cache data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setim(im)
  im
}
