## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##There first function is makeCacheMatrix
##makeCacheMatrix has set, get, setinv, getinv
##library(MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
inv<-NULL 
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x
  }
  list(set=set, get=get,
  setinv=setinv,
  getinv=getinv)
}


## Write a short comment describing this function
##It will be used to get the cached data
cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)) { ##This is to check if inverse is null
   message("fetching cached data")
return(inv)     
  }
  data<-x$get()
  inv <-solve(data,...)
  x$setinv(inv)
inv  
}
