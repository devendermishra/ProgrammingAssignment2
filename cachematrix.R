## Put comments here that give an overall description of what your
## functions do


##makeCacheMatrix creates a special matrix with list of the following
##function
##1. Set the matrix
##2. Get the matrix
##3. Set the Inverse of the matrix
##4. Get the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	#m is the cached result of solve operation
	m <- NULL
	
	#set and get the matrix respectively
	#matrix

	set <- function(y) {
                x <<- y
                m <<- NULL
        }
	
        get <- function() x

	#setinv and getinv are to set/get the inverse of the matrix
	# or any other result which caller wants to store respectively	

	#Assign the calculated solution
        setinv <- function(solution) m <<- solution
	#Return the calculated solution
        getinv <- function() m


        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##cacheSolve find the inverse of special matrix created by makeCacheMatrix
##If the inverse is already calculated, it gives the precalculated inverse.
##Else, it computes the inverse and stores it in special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()

	#It founds the precomputed inverse
        if(!is.null(m)) {
                message("getting cached matrix inverse")
                return(m)
        }

	#Else, compute the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

###Example: How to test or use?
### c<-rbind(c(1,-1),c(-1,1))
### y<-makeCacheMatrix(c)
### cacheSolve(y)
### On the next call to cacheSolve(y), it will give the matrix inverse from cache.
