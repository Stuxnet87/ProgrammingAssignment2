## Put comments here that give an overall description of what your
## functions do

## creates a list of 4 functions to set matrix, get matrix, set inverse and get inverse of matrix
## example: matsample<-makeCacheMatrix(); matsample$setMatrix(matrix variable here)

makeCacheMatrix <- function(x = matrix()) {

	    InvX <- NULL
		## function sets the matrix & makes inverse null since matrix might change
        setMatrix <- function(y) {
                x <<- y
                InvX <<- NULL
        }
        ## returns the matrix
		getMatrix <- function() x
		## called by cacheSolve to set the inverse of matrix and put it in InvX variable
        setinv <- function(inv) InvX <<- inv
		## used to get inverse of matrix set through setMatrix
        getinv <- function() InvX
		## list used to define the function calling names
        list(setMatrix = setMatrix, getMatrix = getMatrix, setinv = setinv, getinv = getinv)
}


## Computes the inverse of matrix, checks for cached data & shows that if present , examples usage: cacheSolve(matsample)
cacheSolve <- function(x) {
		## Check if the matrix inverse exists in cache	
        InvX <- x$getinv()
        if(!is.null(InvX)) {
                message("Cached data received")
                return(InvX)
        }
		## If matrix inverse doesn't exist in cache, then compute inverse
		Mat=x$getMatrix()
		## Flag checks if matrix is invertible by checking whether no of rows = no of columns and determinant of matrix is greater than 0
		flag<-(nrow(Mat)==ncol(Mat))&(det(Mat)!=0)
        ## If flag is true then invert matrix, otherwise show error message
		if (flag)
		{
		  InvX <- solve(Mat)
		}
		else
		{
		  InvX<-"Matrix not invertible"
		}
        x$setinv(InvX)
        InvX
}
