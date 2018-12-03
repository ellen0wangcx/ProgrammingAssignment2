## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(original.matrix = matrix()) {
  #Check if we have correct input
  if (!is.matrix(original.matrix)) {
    stop('Please give a matrix')
  }
  #Build invert matrix
  inverted.matrix<-NULL
  set<-function(y) {
    original.matrix<<-y
    inverted.matrix<<-NULL
  }
  #Functions for getting and setting cached invert matrix value
  get<-function() original.matrix
  #Inversing the matrix using build in solve function
  set.inverse<-function(solve) inverted.matrix<<-solve
  get.inverse<-function() inverted.matrix
  list(set=set, get=get,
       set.inverse=set.inverse,
       get.inverse=get.inverse)
}


## Write a short comment describing this function
##Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
##If the inverse has already been calculated and there's no change in the matrix then the cacheSolve() returns the cached inverse 

cacheSolve <- function(cacheable.matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted.matrix<-cacheable.matrix$get.inverse()
  #If cached matrix is available
  
  if(!is.null(inverted.matrix)) {
    message("getting cached inverse matrix")
    return(inverted.matrix)
  }
  #Creat inverted matrix in case
  #There's no cached matrix available
  matrix.to.inverse<-cacheable.matrix$get()
  inverted.matrix<-solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix

}

