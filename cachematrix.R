makeCacheMatrix <- function( matriz = matrix() ) {

    inv <- NULL

    def <- function( matrix ) {
            matriz <<- matrix
            inv <<- NULL
    }

    get <- function() {
    	matriz
    }

    defInversa <- function(inverse) {
        inv <<- inverse
    }

    getInverse <- function() {
        inv
    }

    list(def = def, get = get,
         defInversa = defInversa,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {

    matriz <- x$getInverse()

    if( !is.null(matriz) ) {
            message("getting cached data")
            return(matriz)
    }

    data <- x$get()

    matriz <- solve(data) %*% data

    x$defInversa(matriz)

    matriz
