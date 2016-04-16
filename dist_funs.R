euclid <- function(x, y) {
    stopifnot(length(x) == length(y))
    sqrt(sum((y - x) ^ 2))
}

manhattan <- function(x,y) {
    stopifnot(length(x) == length(y))
    sum(abs(y-x))
}

distance <- function(x, y, distfun = euclid) {
    if(missing(x) || missing(y)) stop("\"x\" and \"y\" must be specified")
    useMethod("distance", x, y = y, distfun = distfun)
}

distance.vector <- function(x, y, distfun = euclid) {
    distfun(x,y)
}

distance.data.frame <- function(x, y, distfun = euclid) {
    if (!all(sapply(x, is.numeric)))
        stop("x must be entirely numeric. If necessary co-erce non-numerics.")
    NextMethod(data.matrix(x), y, distfun)
}

distance.matrix <- function(x, y, distfun = euclid, apply) {
    if (is.matrix(y)) {
        stopifnot(nrow(x) == nrow(y))
        ## Make matrix. As many rows as there are centroids, as many 
        ## columns as there are samples    
        apply(x, 2, function(col) {
            apply(y, 2, function(row) {
                distfun(col, row)
            })
        })
    } else if (is.vector(y)) {
        apply(x, 2, function(col) {
            sapply(y, function(row) {
                distfun(col, row)
            })
        })
    } else stop("Unsupported format for y")
}
