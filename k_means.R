## Major todo: t() everything, normal matrix format not exprseset format
k_means <- function(data_matrix, k, max_iter = 10^6, distfun = euclid) {
    original_data_matrix <- data_matrix
    if(is.data.frame(data_matrix) && !all(sapply(data_matrix, is.numeric))) {
        cat("data_matrix is not all numeric...\nCoercing to numeric\n")
        char_cols <- sapply(data_matrix, is.character)
        data_matrix[, char_cols] <- lapply(data_matrix[, char_cols], 
            function(col) as.factor(col))
        data_matrix <- data.matrix(data_matrix) 
    }
    #initialising local variables
    samples <- nrow(data_matrix)
    features <- ncol(data_matrix)
    assert_that(k < samples)
    centroid_matrix <- data_matrix[sample(samples, k), ]
    SSE <- 0
    iter <- 1
    while(iter < max_iter) { 
        if (iter > 1) old_membership_vector <- membership_vector
        
        distance_matrix <- distance(data_matrix, centroid_matrix, distfun)
        membership_vector <- apply(distance_matrix, 2, which.min)

        old_centroid_matrix <- centroid_matrix
        centroid_matrix <- t(sapply(sort(unique(membership_vector)), 
            function(i) {
                colMeans(data_matrix[membership_vector == i, ])
            }
        ))

        for (i in 1:k) {
            distance_moved <- fields::rdist.vec(
                old_centroid_matrix, 
                centroid_matrix
            )
        }

        oldSSE <- SSE
        SSE <- 0
        #SSE calculation
        ## Probably rewrite entirely, this is garbage
        SSE <- sum(apply(centroid_matrix, 1, 
            function(centroid ) {
                sapply(unique(membership_vector) function(j) {
                    distfun(data_matrix[membership_vector == j, ], centroid) ^ 2
                })
            })
        )

        iter <- iter + 1
        if (iter >= 3) {
            if ((oldSSE - SSE) < 0.01) || 
                distance_moved < 0.01 || 
                identical(membership_vector, old_membership_vector)) {
                structure(list(
                    "membership_vector" = membership_vector,
                    "data_matrix" = original_data_matrix,
                    "k" = k,
                    "iterations" = iter,
                    "totalsse" = sse,
                    "betweensse" = sse),
                    class = c("k_means", "clusterobject"))
                return(out)
            }
        }
    }
    structure(list(
        "membership_vector" = membership_vector,
        "data_matrix" = original_data_matrix,
        "k" = k,
        "iterations" = iter,
        "totalsse" = sse,
        "betweensse" = sse),
        class = c("k_means", "clusterobject"))
}


# totss   
# The total sum of squares.

# withinss    
# Vector of within-cluster sum of squares, one component per cluster.

# tot.withinss    
# Total within-cluster sum of squares, i.e. sum(withinss).

# betweenss   
# The between-cluster sum of squares, i.e. totss-tot.withinss.

# size    
# The number of points in each cluster.

# iter    
# The number of (outer) iterations.


x_means <- function(data_matrix, k, max_k, distfun = euclid) {
    assert_that()
}




