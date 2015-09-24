makeCacheMatrix <- function(x = numeric()) {
  matrix <- NULL
  set <- function(y){
    x<<-y
    matrix<<-NULL
  }
  get<-function()x
  setInverse<-function(solve)matrix<<-solve
  getInverse<-function()matrix
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

cacheSolve<-function(x,...){
  matrix<-x$getInverse()
  if(!is.null(matrix)){
    message("getting cached data")
    return(matrix)
  }
  data<-x$get()
  matrix<-solve(data,...)
  x$setInverse(matrix)
  matrix
}