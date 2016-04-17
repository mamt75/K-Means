The AIC can be calculated with the following function:
    kmeansAIC = function(fit){

    m = ncol(fit$centers)
    n = length(fit$cluster)
    k = nrow(fit$centers)
    D = fit$tot.withinss
    return(D + 2*m*k)
    }

# From the help for stats::AIC, you can also see that the BIC can be calculated in a similar way to the AIC. An easy way to get the BIC is to replace the return() in the above function, with this:

return(data.frame(AIC = D + 2*m*k,
                  BIC = D + log(n)*m*k))

# So you would use this as follows:

fit <- kmeans(x = data,centers = 6)
kmeansAIC(fit)


BIC.k_means


logLik.kmeans <- function(object) structure(
    object$tot.withinss,
    df = nrow(object[["centers"]]) * ncol(object[["centers"]]),
    nobs = length(object[["cluster"]])
)

logLik.k_means <- function(object) structure(
    object$tot.withinss,
    df = nrow(object[["centers"]]) * ncol(object[["centers"]]),
    nobs = length(object[["cluster"]])
)


# http://stackoverflow.com/questions/15839774/how-to-calculate-bic-for-k-means-clustering-in-r
