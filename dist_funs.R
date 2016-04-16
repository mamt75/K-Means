euclid <- function(x, y) {
    assert_that(length(x) == length(y))
    sqrt(sum((y - x) ^ 2))
}

manhattan <- function(x,y) {
    assert_that(length(x) == length(y))
    sum(abs(y - x))
}

distance <- function(x, y, distfun = euclid) {
    if(missing(x) || missing(y)) stop("\"x\" and \"y\" must be specified")
    UseMethod("distance")
}

distance.vector <- function(x, y, distfun = euclid) {
    distfun(x, y)
}

distance.data.frame <- function(x, y, distfun = euclid) {
    if (!all(sapply(x, is.numeric)))
        stop("x must be entirely numeric. If necessary co-erce non-numerics.")
    NextMethod(data.matrix(x), y, distfun)
}

distance.matrix <- function(x, y, distfun = euclid) {
    if (is.matrix(y)) {
        assert_that(ncol(x) == ncol(y))
        ## Make matrix. As many rows as there are centroids, as many 
        ## columns as there are samples    
        apply(x, 1, function(i) {
            apply(y, 1, function(j) {
                distfun(i, j)
            })
        })
    } else if (is.vector(y)) {
        apply(x, 1, function(i) {
            sapply(y, function(j) {
                distfun(i, j)
            })
        })
    } else stop("Unsupported format for y")
}
