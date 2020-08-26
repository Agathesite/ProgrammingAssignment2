## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                   }
  get<-function()x                           ##get matrix
  setInverse<-function(inverse)inv<<-inverse
  getInverse<-function(){
                        inver<-ginv(x)
                        inver%*%x         ##function to obtain inverse
                        }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)                      
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
  inv<-x$getInverse()
  if(!is.null(inv)){
                    message("getting cached data")
                    return(inv)
  }
  data<-x$get() 
  inv<-solve(data,...)
  x$setInverse(inv)
}      ## Return a matrix that is the inverse of 'x'


