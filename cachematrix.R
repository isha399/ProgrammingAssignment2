## This function creates a special matrix object that can cache its inverse

makecachematrix <- function(x=matrix()){
  inverse <- NULL
  set <- function(m){
    x <<- m
    inverse <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){inverse<<-inverse}
  getInverse <- function(){inverse}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}



## This function computes the inverse of the cashed matrix object returned by the above function

cachesolve <- function(x,...){
  inverse <- x$getInverse()        ## Return a matrix that is the inverse of 'x'
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
}