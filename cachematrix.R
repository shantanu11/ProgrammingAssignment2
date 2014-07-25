
makeCacheMatrix <- function(x = matrix()) 
  {
  
   m<-NULL
   set<-function(y)  
    {
     #Assign a new matrix
      x<<-y
      m<<-NULL
    }
   get<-function()
     {
     #Return the matrix that was passed into the function
     #or set by set(y)
       return(x)
     }
   setmatrix<-function(solve) 
     m<<- solve                  #Assign the inverse of the y matrix
   getmatrix<-function() m       #Return the inverse of the matrix
   list(set=set, get=get,        # Return the four embedded functions in a list so they can be manipulated
   setmatrix=setmatrix,
   getmatrix=getmatrix)
  }


cacheSolve <- function(x, ...) 
  {
  # First, check to see if the inverse has been calculated
  # If not, calculate and set it.
  m<-x$getmatrix()
  if(!is.null(m))
    {
    # Get the inverse
    message(" cached data")
    return(m)
    }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m             

        ## Return a matrix that is the inverse of 'x'
}
