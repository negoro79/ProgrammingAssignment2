makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ## setting the first function
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## getting the matrix
  get<-function() x
  ## setting the inverse of the matrix
  setmatrix<-function(solve) m<<- solve
  ## getting the inverse of the matrix
  getmatrix<-function() m
  ## the list of four functions
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  ## getting cached matrix
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## calculating a new meaning
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}