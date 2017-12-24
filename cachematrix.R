## two functions to cache the inverse of a matrix

## this first function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    invers <- NULL   ##Initialization step
    ##set method: set the matrix
    set <- function(matr){
        x <<- matr
        invers <<- NULL
    }
    
    ##get the matrix
    get <- function(){x}
    
    
    ##method to set the inverse of the matrix 
    setInvers <- function(inver_mat){
        invers <<- inver_mat
     
    }
    ##method to get (return) the inverse of the matrix
    getInvers <- function(){
        invers
    }
    
    ## return a list of methods
    list(set=set, get=get, setInvers=setInvers, 
         getInvers=getInvers )
}


## this second fucntion calculate the inverse of a matrix 
## returned by 'makeCatchmatrix' above. If the inverse has already been calculated 
##(and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    invers <- x$getInvers()
    ## return the inverse if it exists already
    if(!is.null(invers)){
        message("getting catched data")
        return(invers)
    }
    
    ## get the matrix from the object 'x'
    my_matrix <- x$get()
    
    ## calculate the inverse by matrix multiplication
    invers <- solve(my_matrix) %*% my_matrix
    
    ## set the inverse to the object 'x'
    x$setInvers(invers)
    
    ##return the inverse matrix
    invers
}

