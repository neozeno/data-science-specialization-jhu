# =============================================================================
# Cache Matrix Inversion
# =============================================================================
# This file implements a cache-aside pattern for matrix inversion in R,
# exploiting lexical scoping and the <<- operator to persist state inside
# closure-based objects across multiple function calls.
#
# The problem being solved: matrix inversion via solve() is computationally
# expensive — O(n^3) in the general case. If the same matrix needs to be
# inverted repeatedly (e.g. inside a loop or iterative algorithm), recomputing
# the inverse every time wastes CPU cycles. The solution is to compute it once,
# store the result in a cache that lives inside the object, and return the
# cached value on every subsequent call — as long as the matrix has not changed.
#
# How it works:
#   makeCacheMatrix(x) wraps a matrix inside a closure that carries two pieces
#   of state — the matrix itself and its inverse — in a private enclosing
#   environment. It exposes four accessor functions (set, get, setinv, getinv)
#   as a named list. The <<- operator inside those functions writes back to the
#   enclosing environment rather than creating transient local variables, which
#   is what allows the cached inverse to survive across calls.
#
#   cacheSolve(x) is the cache-aware compute function. It first asks the object
#   whether a cached inverse already exists. On a cache hit it returns that
#   value immediately without touching solve(). On a cache miss it pulls the
#   raw matrix, calls solve(), stores the result back into the object's cache
#   via setinv(), and returns it. If the underlying matrix is later replaced
#   via $set(), the cache is automatically invalidated (reset to NULL) so stale
#   inverses are never returned.
#
# Usage:
#   cm <- makeCacheMatrix(matrix(c(4,3,3,2), nrow = 2))
#   cacheSolve(cm)   # computes and caches the inverse
#   cacheSolve(cm)   # returns cached result, skips computation
#   cm$set(matrix(c(1,2,3,4), nrow = 2))  # swap matrix, cache invalidated
#   cacheSolve(cm)   # recomputes for the new matrix
# =============================================================================


# -----------------------------------------------------------------------------
# makeCacheMatrix
# -----------------------------------------------------------------------------
# Creates a special "matrix" object — really a closure that bundles the matrix
# together with its cached inverse and four accessor functions.
#
# The trick: x and inv live in THIS function's environment (the enclosing env
# of the four inner functions). <<- writes back to that enclosing environment
# instead of creating a local variable, so state persists across calls.
#
# Returns a list of four functions:
#   $set(y)     — store a new matrix, invalidate the cached inverse
#   $get()      — retrieve the stored matrix
#   $setinv(i)  — write the computed inverse into the cache
#   $getinv()   — read the cached inverse (NULL if not yet computed)

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL  # cache starts empty every time a new object is created

        set <- function(y) {
                x   <<- y     # replace the stored matrix in the enclosing env
                inv <<- NULL  # new matrix → old inverse is invalid, reset cache
        }

        get <- function() x   # simply returns whatever x is in the enclosing env

        setinv <- function(solved) inv <<- solved  # write computed inverse to cache

        getinv <- function() inv  # return cache (NULL means not yet computed)

        list(
                set    = set,
                get    = get,
                setinv = setinv,
                getinv = getinv
        )
}


# -----------------------------------------------------------------------------
# cacheSolve
# -----------------------------------------------------------------------------
# Computes (or retrieves) the inverse of the special matrix object created by
# makeCacheMatrix().
#
# Strategy (cache-aside pattern):
#   1. Ask the object for its cached inverse.
#   2. If the cache is populated (non-NULL), return it immediately — no work done.
#   3. If the cache is empty, pull the raw matrix, invert it with solve(),
#      push the result back into the cache, then return it.
#
# Arguments:
#   x   — a makeCacheMatrix object
#   ... — extra args forwarded to solve() (e.g. tolerance settings)
# -----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {

        inv <- x$getinv()  # step 1: check cache

        if (!is.null(inv)) {
                message("retrieving inverse from cache — skipping computation")
                return(inv)  # step 2: cache hit, return early
        }

        # step 3: cache miss — compute, store, return
        mat <- x$get()
        inv <- solve(mat, ...)  # solve(A) returns A^-1 for a square matrix
        x$setinv(inv)           # populate the cache for next time
        inv
}

# Walkthrough with a concrete example

# Create a 2x2 invertible matrix
A <- matrix(c(4, 3, 3, 2), nrow = 2)

# Wrap it in the caching object
cm <- makeCacheMatrix(A)

# First call — cache is empty, solve() runs
cacheSolve(cm)
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4

# Second call — prints the message, returns cached result instantly
cacheSolve(cm)
# retrieving inverse from cache — skipping computation
#      [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4

# Swap in a new matrix — cache is automatically invalidated
cm$set(matrix(c(1, 2, 3, 4), nrow = 2))
cacheSolve(cm)   # recomputes from scratch, no message
