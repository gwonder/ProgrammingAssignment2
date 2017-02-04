## $set() caches the original matrix value passed in
## $get() retrieves original matrix values cached
## $getissame() returns a boolean indicating if original matrix values have 
##    changed since inverse was cached
## $setminv() caches inverse matrix created from solve function of $get matrix value
## $getminv() returns last cached inverse matrix set

## This function creates a special matrix object that can cache its original value
## and the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {

  minv <- NULL
  issame <- F
  set<-function(y) {
    #print("set matrix")
    issame<<-F
    x<<-y
    minv <<- NULL
  }
  get<-function()x
  # setissame<-function(s = FALSE) issame<<-s
  getissame<-function()issame
  setminv<-function(m) {
    minv<<-m
    issame<<-T
  }
  getminv<-function() minv
  list(set = set, get = get, getissame = getissame, setminv = setminv, getminv = getminv)
}


## This function computes the inverse of the special square matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$get()
  if (!is.null(m)) {
    #print("cached matrix exists")
    minv<-x$getminv()
    if ((x$getissame()) &&(!is.null(minv))) {
      message("getting cached inverse matrix")
      return(minv)
    }
    #print("need inverse set")
  }
  minv<-solve(m)
  x$setminv(minv)
  return(minv)

}

# # Tests
# a<-matrix(rnorm(9),3,3)
# b<-makeCacheMatrix(a)
# #Creates and sets matrix inverse
# cacheSolve(b)
# #Returns cached matrix inverse
# cacheSolve(b)
# #Sets new matrix value of makeCacheMatrix
# b$set(matrix(rnorm(4),2,2))
# #Detects new matrix values and sets new inverse matrix
# cacheSolve(b)
# #Returns cached inverse matrix values
# cacheSolve(b)