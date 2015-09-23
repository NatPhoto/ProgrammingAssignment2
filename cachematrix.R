## This R file consists of two functions, makeCacheMatrix and cacheSolve.  The first creates 
## a special "matrix" object that can cache its inverse. Then the cacheSolve computes the inverse of
## the special "matrix".

## makeCacheMatrix is a funciton that stores a list of functions set, get, setINV, and getINV. No
## calculation of the inverse is performed.  

makeCacheMatrix<-function(x=matrix()) {
  
  #set, get, setINV, and getINV are stored as objects in a list.  These will be used as inputs in
  #cacheSolve().
  
  m=NULL
  set=function(y) {
    x<<-y
    m<<-NULL
  }
  get=function() x
  setINV=function(Inver) m<<-Inver         
  getINV=function() m
  list(set=set, get=get, setINV=setINV, getINV=getINV)
}

## cacheSolve will evalute the input and compare it against the cached value stored from makeCacheMatrix
## and if not found it will take the inverse of the matrix.

cacheSolve<-function(x,...) { 
  m=x$getINV()
  
  #If the inverse is stored in cache it will get it will print the message and return the value
  #and skips the calculation.
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #this will calculate the inverse of the matrix if the above is false.
  
  data=x$get()
  m=solve(data,...)
  x$setINV(m)
  return(m)
}
