## These functions provide a mechanism for caching the inverse of a matrix
## and returning this cached value if it has previously been calculated

## makeCacheMatrix creates an artificial structure to hold a matrix and its inverse,
## and it defines 4 functions necessary to maniupulate these.
## Its formal structure is a list of these 4 functions

## To begin using this construct, create your new 'artificial matrix' as follows
## my_dummy_matrix <- makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialise the inverse to indicate it hasn't yet been calculated
  
    m_inv <- NULL 
    
    ## first function : set()
    ## the set function is used to put the original matrix into the structure, e.g.
    ## my_dummy_matrix$set(my_original_matrix)
    
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    
    ## second function : get()
    ## you can retrieve the original matrix using the get function
    ## my_dummy_matrix$get()
    
    get <- function() x
    
    ## third function : setinv()
    ## when the inverse is calculated for the first time it is stored using setinv
    ## e.g. using my_dummy_matrix$setinv( the calculated value of the inverse )
    
    setinv <- function(inv) m_inv <<- inv
    
    ## fourth function : getinv()
    ## the stored value of the inverse is retrieved using getinv
    ## e.g. somewhere_to_store_it <- my_dummy_matrix$getinv()
    
    getinv <- function() m_inv
    
    ## the makeCacheMatrix function returns a list of these 4 functions
    
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)

}


## cacheSolve() is a special version of the standard R function solve()
## it uses solve() to calculate the inverse unless it has already done so,
## in which case it returns a cached version without re-calculation

cacheSolve <- function(x, ...) {
  
        ## first, attempt to retrieve the cached version of the inverse
  
        m_inv <- x$getinv()
        
        ## check to see if there is already a cached version, 
        ## and if so return this without re-calculating it
        
        if(!is.null(m_inv)) {
            message("getting cached data")
            return(m_inv)
        }
        
        ## otherwise, retrieve the orignial matrix using get(),
        ## calculate its inverse using the standard function solve()
        ## and store it in the artificial structure using setinv()
        
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinv(m_inv)
        
        ## Return a matrix that is the inverse of 'x'
    
        m_inv
}
