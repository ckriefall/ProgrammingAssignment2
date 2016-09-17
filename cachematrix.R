## build two functions to cache a matrix, cache corresponding inverse
## makeCacheMatrix has a list of functions get, set, setinv, getinv to store and retrieve matrix and inverse
## cacheSolve creates the inverse matrix and stores it in cache to use multiple times without recreating 

## NOTES
## Functions are based on the vector caching example given in the assignment
## For single line functions, I added curly braces for consistency and readability
## Verbose comments were added for clarity (almost one comment per line)

makeCacheMatrix <- function(x = matrix()) {	# assign function name of makeCachMatrix that accepts a matrix argument
  m <- NULL                   # empty the inverse when setting the matrix
  set <- function(y) {				# define set function, makeCache$set()
    x <<- y			    		      # create cached value of x
    m <<- NULL					      # cached m is NULL (any previous cache is not based on this new matrix)
  }							
  get <- function() {x}				# single line function returns the matrix
  setinverseMatrix <- function(invMatrix) {m <<- invMatrix}	# cache inverse matrix (cacheSolve puts result here)
  getinverseMatrix <- function() {m}	  # retrieve cached m
  list(									# create list of functions: set, get, setinv and getinv
    set = set,
    get = get,
    setinv = setinverseMatrix,
    getinv = getinverseMatrix)
}

## cacheSolve creates the inverse matrix using solve function the first time it is called 
## and stores inverse matrix in the cache using the setinv function using setinv created above
## cacheSolve retrieves the inverse matrix if it already exists using getinv function created above
## cacheSolve returns the inverse matrix whether it is newly created or already exists
## an additional message will alert user if the inverse matrix already cached

cacheSolve <- function(x, ...) {
  m <- x$getinv()				              # get the cached matrix
  if(!is.null(m)) {					          # does it exist?
    message("getting cached data")		# message to screen that cached version exists
    return(m)					                # show the cached inverse matrix
  }
  else {                    # else and curly braces for readability
    data <- x$get()					# load matrix from cache
    m <- solve(data, ...)		# pass the parameter (cached matrix) to the function solve()
    x$setinv(m)		          # store newly created inverse matrix in the cache
    m							          # show the newly stored inverse matrix
  }
}
