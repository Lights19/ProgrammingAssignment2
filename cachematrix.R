
#create a special "matrix" 

makeCacheMatrix  <- function(x=matrix()){
  
  #stores cached value
  inv  <- NULL
  
  #create the matrix       
  set  <-function(y){
    x   <<-y
    inverse <<-NULL
    
  }
  get   <-function()x
  setinverse <- function(inverse){
    
    inv <<- inverse
  }
  getinverse <- function(){
    inv
  }
  list(set=set,get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
  
}



#computes the inverse of the special"matrix" created in makeCacheMatrix

cacheSolve  <- function(x,...)  {
  inv      <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)    ## display matrix
  }
  mat  <-x$get()
  inv  <-solve(mat,...)
  x$setinverse(inv)
  inv
}