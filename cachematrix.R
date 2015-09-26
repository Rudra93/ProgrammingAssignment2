##Create a function with a null matrix initialized
makeCacheMatrix <- function(x=matrix())
{
	##Initialize inverse to null initially
	i <- NULL
	set <- function(y)
	{
		## Set the value of matrix to work outside its environment
		x <<- y
		## Set inverse as null if new matrix created or value of matrix changed
		i <<- NULL
	}
	## Create a function to get the value of matrix
	get <- function() x

	## Initialize inverse variable to the value passed in setinverse function
	setinverse <- function(inverse)
	{
		i <<- inverse
	}
 
      ## Get the value of inverse matrix
	getinverse <- function() i
	
	## Output of makeCacheMatrix function
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Function to calculate inverse if not cached
cacheSolve <- function(x)
{

	## Get inverse value
	i <- x$getinverse()
	## If inverse exists return cached data
	if(!is.null(i))
	{
		message("Getting cached data")
		return(i)
	}
	## Load data using get function
	data <- x$get()
	## Calculate inverse
	i <- solve(data)
	## Set the value of inverse 
	x$setinverse(i)
	## Return the inverse value as output
	i
}