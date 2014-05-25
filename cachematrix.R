## These functions provide a list 4 methods set , get , setinverse and getinverse 
## to store inverse of a matrix in cache and to return the solved matrix back when called


##makeCacheMatrix provides a list of functions 
##get , set , getinverse and setinverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() {x}
  setinverse<-function(solve) {m<<- solve}
  getinverse<-function() {m} 
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)  
}


# Computes, caches, and returns matrix inverse
cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("geting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}
