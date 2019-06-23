##programming assignment 2
##create a super matrix to store it's context and it's inverse

##create makeCacheMatrix function to cache a matrix
makeCacheMatrix<-function(x=matric()){
  ##initialize the inv variable
  inv<-NULL
  ##create set fuction to assigne y value to x and initialize inv
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  ##create get function to retrieve x
  get <-function() x
  ##create set function to assigne inv variable
  setinv<-function(inverse) inv<<-inverse
  #creat getinv function to retrieve inv
  getinv<-function() inv
  ##create a list that contains 4 functions
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}



##create cachesolve function to solve the inverse of a matrix
cacheSolve<-function(x,...){
  ##assigne the inverse value of matrix x to inv
  inv<-x$getinv()
  ##if inv is not null, return inv along with a message
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ##if inv is null, caculate the inverse of the matrix
  ##assigne matrix value to data
  data<-x$get()
  ##use solve function to invert a matrix
  inv<-solve(data,...)
  ##get the inverse of matrix
  x$setinv(inv)
  ##return the inverse of matrix
  inv
}

##set the matrix value
m<-matrix(rnorm(25),5,5)
##catch the matrix
x<-makeCacheMatrix(m)
##get the inverse of the matrix
cacheSolve(x)

