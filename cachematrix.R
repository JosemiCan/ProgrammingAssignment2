## the functions below cache the inverse of a matrix

## the first function creates a matrix object that cahe its inverse

makeCacheMatrix<-function(m=matrix())
  {
  n<-NULL
  set<-function(x)
  { m<<-x
  n<<-NULL }
  get<-function()m
  setinverse<-function(inverse) n <<-inverse
  getinverse<-function() n
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)}


## the second function computes the inverse of the matrix returned by the other function
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve<-function(m,...)
  {
  n<-m$getinverse()
  if(!is.null(n))
  {  message("getting cached data")
  return(n)  }
  data<-m$get()
  n<-solve(data)%*%data
  m$setinverse(n)
  n  }
