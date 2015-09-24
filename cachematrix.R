# Defining the makeCacheMatrix

makeCacheMatrix <- function(x = numeric()) {
  matrix <- NULL
  set <- function(y){                         #define subfunction set
    x<<-y
    matrix<<-NULL
  }
  get<-function()x
  setInverse<-function(solve)matrix<<-solve   #R-function solve is the base function used in the subfunction setInverse
  getInverse<-function()matrix
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

#Defining CacheSOlve
cacheSolve<-function(x,...){
  matrix<-x$getInverse()               
  if(!is.null(matrix)){                      #Check if it exists in the cached
    message("getting cached data")
    return(matrix)                           #If exists, print message and directly use the one in cahce
  }
  data<-x$get()
  matrix<-solve(data,...)
  x$setInverse(matrix)
  matrix
}