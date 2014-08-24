# This short program is written to find the inverse
# of a matrix and then cache that result for later use

# makeCacheMatrix function
# establishes set, get, setinverse, and getinverse functions

makeCacheMatrix<-function(x = matrix()){
      m<-NULL
      set<-function(y){
            x<<-y
            m<<-NULL
      }
      get <-function()x
      setinverse<-function(solve) m<<- solve
      getinverse<-function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve function
# returns a matrix that is the inverse of 'x'

cacheSolve<-function(x,...){
      m<-x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data<-x$get()
      m<-solve(data,...)
      x$setinverse(m)
      m
}
