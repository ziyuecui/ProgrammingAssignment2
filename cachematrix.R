######## The following functions: makeCacheMatrix, cacheSolve
########  take advantage of the scoping rules to store the inverse of a matrix


## takes a matrix and returns a list
makeCacheMatrix <- function(x = matrix()) {
        inverse_holder <- NULL # the holder is used to transfer info
        
        set.matrix <- function(new_matrix) { # set.matrix can take in new matrix 
                x <<- new_matrix
                inverse_holder <<- NULL
        }
        get.matrix <- function() x 
        
        set.inverse <- function (inverse) { # assign inverse value to the holder
                inverse_holder <<- inverse
        }
        get.inverse <- function() inverse_holder # return the holder
        
        list(set.matrix = set.matrix,
             inverse = get.inverse,
             get.matrix = get.matrix,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## cacheSolve create the inverse of a matrix and upload it to cache
## if there is already an inverse calculate, that inverse will be returned

cacheSolve <- function(x, ...) {
        if(is.null(x$get.inverse())) { # when an inverse is yet to be calculated
                message('new cache stored')
                inverse <- solve(x$get.matrix())
                x$set.inverse(inverse)
                x$get.inverse()
        } else { # if the inverse has been calculated and stored
                message("load cache")
                x$get.inverse()
        }
}



####### Testing, make sure the functions deliever
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
dt <- makeCacheMatrix(m1)
dt$get.matrix()
dt$get.inverse()
cacheSolve(dt)
cacheSolve(dt)

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
dt$set.matrix(n2)
cacheSolve(dt)
cacheSolve(dt)



