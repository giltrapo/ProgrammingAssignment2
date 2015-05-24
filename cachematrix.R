## These functions are designed with the aim of not having to calculate
## twice the inverse of the same matrix in the same session of R.
## Non repeated matrix are stored in order to know what inverses were
## already calculated.


## This function verify that the entered object is a square matrix,
## checks if it was previously entered into the current R session and,
## not being the case, stores it.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Checks that the cache to store the matrixes and their inverses exist and,
        ## if not, creates it.
        
        if (!exists("wm")) {
                wm <<- list()
                wi <<- list()
        }
        
        ## Checks that x is a square matrix.
        
        if (class(x) == "matrix" && nrow(x) == ncol(x)) {
                
                ## Cheks that matrix is stored.
                
                if (any(sapply(wm, function(y) identical(y, x)))) {
                        pos <-  which((sapply(wm, function(y) identical(y, x)))==TRUE)
                        mtrx <- wm[[pos]]
                } else {
                        mtrx <- x
                        pos <- length(wm) + 1
                        wm[[pos]] <<- x
                        wi[[pos]] <<- NA
                }
        } else {
                message("Please, enter a square matrix")
                mtrx <- NULL
                pos <- NULL
        }
        
        ## Retrieves matrix to calculate its inverse using
        ## cacheSolve function.
        
        getmatrix <- function () mtrx
        
        ## Retrieve position to recover the already calculated inverse using
        ## cacheSolve function.
        
        getpos <- function () pos
        
        
        list(getmatrix = getmatrix, getpos = getpos)
}


## This function either looks for the entered inverse matrix or
## calculates it if not previously calculated.

cacheSolve <- function(x, ...) {
        
        pos <- x$getpos()
        mtrx <- x$getmatrix()
        
        if(is.na(wi[pos])) {
                inv <- solve(mtrx)
                wi[[pos]] <<- inv
                inv
        } else {
                inv <- wi[pos]
                message("Getting cached data")
                inv
        }
}
