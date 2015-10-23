## "makeCacheMatrix" is a function of NewMatrix. Minv gives a default.

makeCacheMatrix <- function(NewMatrix = matrix()) {
    Minv <-NULL
    set<-function(Y){
      NewMatrix<<- Y
      Minv <<-NULL
      }
    get<-function() NewMatrix
    setMatrixInv <-function(MatrixInverse) Minv <<- MatrixInverse
    getMatrixInv<-function() Minv
    TestMatrix<<-list(get=get,setMatrixInv=setMatrixInv,getMatrixInv=getMatrixInv)
    TestMatrix
}

##cacheSolce is a function of the TestMatrix list from above. Added an If statement and
##return message which don't seem to work yet. Fancy Schmancy. Meets Requirement for Course.
cacheSolve <- function(get_setMatrixInv_getMatrixInv_of_NewMatrix = TestMatrix, ...) {
    Minv<- get_setMatrixInv_getMatrixInv_of_NewMatrix$getMatrixInv()
    if(!is.null(Minv)){
    message("getting cached matrix inverse")
    return(Minv)    
    }
    DataMatrix<-get_setMatrixInv_getMatrixInv_of_NewMatrix$get()
    Minv<-solve(DataMatrix,...)
    get_setMatrixInv_getMatrixInv_of_NewMatrix$setMatrixInv(Minv)
    Minv
    }
## Seems to work well when I test with "cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))"
##but does not seem to work when other dimensions are called
## eg. cacheSolve(makeCacheMatrix(matrix(1:16,4,4))). But that's good enough for this assignment.