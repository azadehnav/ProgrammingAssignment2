makeCacheMatrix<- function(x=matrix()){
  i<- NULL 
  set<- function(y){
    x<<- y
    i<<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) i<<- inverse
  getinverse<- function () i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)

}

casheSolve <- function(x, ...){
  i<-x$getinverse()
  if(!is.null(i)){
    message('getting cashed i')
    return(i)
    
  }
  i<- x$get()
  i<- solve(i,...)
  x$setinverse(i)
  i
}

(mat<- makeCasheMatrix(matrix(1:4, nrow = 2, ncol = 2)))
mat$get()
mat$getinverse()
#matrix not cashed yet
casheSolve(mat)
mat$getinverse()
