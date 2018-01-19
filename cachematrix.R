##The functions are designed to show the principle of closure i.e. writing a function within a function
##These functions created below save computational time by creating special matrix that can cache 
##its inverse and need not recalculate it if the matrix has not changed

##This first function, makeCacheMatrix creates a "special matrix" object that can cache its inverse
##It returns list containing functions that help get the inverse of the special matrix 
##This list then becomes the input to cacheSolve()

makeCacheMatrix<-function(x=matrix()){
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse<-function(inverse) inver<<-inverse
  getInverse<-function() inver
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

##This function calculates the inverse of original matrix that was an input to makeCacheMatrix

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}
