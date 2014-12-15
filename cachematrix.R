## OVERALL
## Together, the functions 'makeCacheMatrix' and 'cacheSolve' work together to 
## create a matrix that is able to cache its inverse and return the inverse 
## (either through a new computation, if running the 1st time, or by retrieving 
## the inverse from the cache)

## FIRST FUNCTION
## This function 'makeCacheMatrix' should create a matrix ('x') object that is 
## able to cache its inverse 

makeCacheMatrix<-function(x=matrix()){
	i<- NULL 
		#i will later be defined as the 'inverse' of matrix 'x'
		# it's reset to NULL every time makeCacheMatrix is called
	set<-function(y){
		x<<-y
		i<<-NULL
		}
		# the above code sets an input vector 'y' and saves it as 'x' (previously 
		# defined matrix) and resets the inverse to NULL whenever a new object
		# (or, matrix 'x') is created
	get<-function(){x} 
		# 'get' this function returns the value of the original matrix 'x'
	setinverse<-function(solve) {i<<-solve}
		#'setinverse' this is called by cacheSolve(), in the the second major 
		# function, when 'cacheSolve' is 1st run and will store the value 
		# using the super assignment
	getinverse<-function(){i} 
		#this should return the cached matrix to cacheSolve() on any subsequent calls
	list (set=set, get=get,
		setinverse=setinverse,
		getinverse=getinverse)
		# this list of internal functions (functions within 'makeCacheMatrix') shows
		# the calling function ('cacheSolve') how to access these functions
	}

## SECOND FUNCTION
## This function 'cacheSolve' should create the inverse matrix of the special matrix 
## 'x' created with 'makeCacheMatrix.' Before calculating the inverse, 'cacheSolve' first 
## checks to see if this inverse matrix has already been created. If it has, it retrieves 
## the inverse from the cache and doesn't recalculate the inverse of matrix 'x'.  
## If it hasn't already been calculated, 'cacheSolve' looks back at 'makeCacheMatrix'
## to get matrix 'x' then calculates the inverse and returns back this inverse ('i').

cacheSolve<-function(x,...){
	i<-x$getinverse() 
		# looks at the matrix 'x' and gets the value of it's inverse ('i')
	if(!is.null(i)){ 
		# if inverse ('i') was prevoiusly calculated (in other words "i" is not null) 
		# then the following message is returned
		message("Hold up... getting chached data")
		return(i) 
			# returns the inverse matrix, ending this function
		}
	data<-x$get() 
		# this should execute if x$getinverse returned a null (in other words, 
		# the inverse of matrix 'x' hasn't already been calculated)
	i<-solve(data,...) 
		# if i was null then this function should recalculate the inverse of matrix 'x'
	x$setinverse(i)
		# should store the inverse in matrix 'x'
	i 
		# should return 'i': the value of the inverse of matrix 'x'
	}
