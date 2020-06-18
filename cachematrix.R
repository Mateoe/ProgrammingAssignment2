#makeCacheMatrix creates a special "matrix" that stores the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL       #The default value of the inverse matrix
    
    set<-function(y){   #Sets the value of the matrix
        x<<-y
        inverse<<-NULL
    }
    
    get<-function() x                                           #Gets the value of the original matrix
    setInverse<-function(inverseMatrix) inverse<-inverseMatrix  #Gets the value of the inverse matrix
    getInverse<-function() inverse                              #Sets the value of the inverse matrix
    
    list(set=set,
         get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


#cacheSolve calculates or retunrs the inverse matrix

cacheSolve <- function(x, ...) {
    inverse<-x$getInverse()             #Gets the value of the inverse matrix
    
    if(!is.null(inverse)){              #If the inverse matrix is not null, it is returned
        message("Getting cache data")
        return(inverse)
    }
    
    data<-x$get()                       #Sets the value of the matrix in data
    inverse<-solve(data, ...)           #The value of the inverse is calculated
    x$setInverse(inverse)               #Saves the value of the inverse matrix
    
    inverse                             #Return the inverse matrix
    
}
