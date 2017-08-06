##The makeCacheMatrix and the cacheSolve functions are used to cache
#the inverse of a matrix

##The makeCacheMatrix function creates a list with functions to
#1.Set the value of the matrix
#2.Get the value of the matrix
#3.Set the inverse of the matrix (for which I have used the solve function)
#4.Get the inverse of the matrix 
#the x matrix is called as an argument in this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(solve)inv<<-solve
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##This function ultimately returns the inverse of the matrix called.
#If the inverse hasn't been computed, it computes the inverse and returns it.

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}

