#The below given function calculates and caches inverse of a matrix
#that are computationally expensive to re-calculate when used multiple times

#the below given function makes a special matrix and returns a list of 
#functions to manipulate them.

makeCacheMatrix <- function(x = matrix()) {
	invMat<-NULL
	set<-function(y){
		x<<-y
		invMat<<-NULL
	}
	get<-function() x
	getInv<-function() invMat
	setInv<-function(inv) invMat<<-inv
	list(set = set, get = get, getInv = getInv, setInv = setInv)	
}


#the function below tries to fetch inverse from memory if already cached
#else computes the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv<-x$getInv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat<-x$get()
	inv<-solve(mat)
	x$setInv(inv)
	inv
}
