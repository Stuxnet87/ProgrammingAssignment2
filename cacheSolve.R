#computes the inverse of matrix, checks for cached data & shows that if present
cacheSolve <- function(x) {
	  # Check if the matrix inverse exists in cache	
        InvX <- x$getinv()
        if(!is.null(InvX)) {
                message("Cached data received")
                return(InvX)
        }
	  # If matrix inverse doesn't exist in cache, then compute inverse
        Mat=x$getMatrix()
	  #Flag checks if matrix is invertible by checking whether no of rows = no of columns and determinant of matrix is greater than 0
	  flag<-(nrow(Mat)==ncol(Mat))&(det(Mat)!=0)
        # If flag is true then invert matrix, otherwise show error message
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