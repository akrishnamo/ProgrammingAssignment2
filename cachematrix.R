## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## create a matrix object

	## initalizes the value of the inverse in the makeCacheMatrix's environment to NULL
	inv<-NULL
	
	##used to retrieve the value of the object i.e. x, from the object's environment
	get<-function() x
	
	##used to set the value of inverse in the object's environment
	setinv<-function(inverse) inv<<-inverse
	
	##used to retreive the value of inverse from the object's environment
	getinv<-function() inv
	
	##return the object after creation
	list(get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	##Return a matrix that is the inverse of 'x'
	
	##retreive the value of inverse from x's environment
	inverse<-x$getinv()
	
	if(!is.null(inverse)){
		   print("returning cached results")
		   return(inverse)}
	else{
		   matrix_data=x$get()
		   inverse<-solve(matrix_data)
		   x$setinv(inverse)
		   print("returning calculated results")
		   inverse
		   }	
}
