makeCacheMatrix <- function(x = matrix()) {
    
       inv <- NULL               #initializing inverse as NULL
       set <- function(y) {
           x <<- y
           inv <<- NULL
         }
      get <- function() {x}           #function to get matrix x
      setInverse <- function(inverse)  {inv<<-inverse}
      getInverse <- function() {inv}     #function to get inverse of the matrix
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }
 
   
   cacheSolve <- function(x, ...)  ##gets cache data
      {
       inv <- x$getInverse()
       if(!is.null(inv)){                   #checking whether inverse is NULL
          message("getting cached inverse")
          return(inv)                      #returns inverse value
       }
      mat <- x$get()
      inv <- solve(mat, ...)              #calculates inverse value
      x$setInverse(inv)
      inv                             ##return a matrix that is inverse of 'x'
     }

   
