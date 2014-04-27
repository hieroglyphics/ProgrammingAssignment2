## These two functions together will give the inverse of an invertible 
## square matrix. If there already exists the inverse in the 
## environment then it will give the result directlywithout going through 
## the computation


## This function creates a special "matrix" object that returns a list of
## functions which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL  ## inverse matrix
  ## Change matrix
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  ## get the matrix
  get <- function() x
  ## set the inverse
  setinv <- function(inv) inver <<- inv
  ## get the inverse
  getinv <- function() inver
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinv()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setinv(inver)
  inver
}
##############################################################
## Example ##
a <- makeCacheMatrix(matrix(c(1, 3, 5, 3, 8, 9, 4, 5, 7), ncol = 3))
## Get the matrix just created
a$get()
#     [,1] [,2] [,3]
#[1,]    1    3    4
#[2,]    3    8    5
#[3,]    5    9    7

## Get the inverse of the matrix ##
a$getinv() ## we didn't calculate yet, so the result is NULL
# NULL

## Calculate the inverse ##
cacheSolve(a)
#            [,1]       [,2]        [,3]
#[1,] -0.3793103 -0.5172414  0.58620690
#[2,] -0.1379310  0.4482759 -0.24137931
#[3,]  0.4482759 -0.2068966  0.03448276

## Calculate the inverse, again! ##
cacheSolve(a)
# getting cached data
#[,1]       [,2]        [,3]
#[1,] -0.3793103 -0.5172414  0.58620690
#[2,] -0.1379310  0.4482759 -0.24137931
#[3,]  0.4482759 -0.2068966  0.03448276

## Now, change the matrix value ##
a$set(matrix(c(1, 2, 6, 3, 5, 8, 2, 5, 7), ncol = 3))
a$get()
#     [,1] [,2] [,3]
#[1,]    1    3    2
#[2,]    2    5    5
#[3,]    6    8    7

## Calculate the inverse for the new matrix, not cached!##
cacheSolve(a)
#          [,1]       [,2]        [,3]
#[1,] -0.3333333 -0.3333333  0.33333333
#[2,]  1.0666667 -0.3333333 -0.06666667
#[3,] -0.9333333  0.6666667 -0.06666667

## Now, calculate again, cached! ##
cacheSolve(a)
#getting cached data
#          [,1]       [,2]        [,3]
#[1,] -0.3333333 -0.3333333  0.33333333
#[2,]  1.0666667 -0.3333333 -0.06666667
#[3,] -0.9333333  0.6666667 -0.06666667
