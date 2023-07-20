# makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix<-function(x=matrix()){
  m<-NULL
# set the value of the matrix
  set_v<-function(y){
    x<<-y
    m<<-NULL
  }
# get the value of the matrix
  get_v<-function(){
    x
  }
# set the inverse of the matrix   
  set_i<-function(solve){
    m<<-solve
  }
# get the inverse of the matrix
  get_i<-function(){
    m
  }
# return the list
  list(set_v=set_v,get_v=get_v,set_i=set_i,get_i=get_i)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
# if the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache
cacheSolve<-function(x,...){
# get the inverse of the matrix
  m<-x$get_i()
# if the inverse of the matrix exists
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
# if the inverse of the matrix doesn't exist 
  data<-x$get_v()
  m<-solve(data)
  x$set_i(m)
  m
}