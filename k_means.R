## Major todo: t() everything, normal matrix format not exprseset format

k_means <- function(data_matrix, k, max_iterations = 10000000000, 
    distfun = euclid) {
    if(is.data.frame(data_matrix) && !all(sapply(data_matrix, is.numeric))) {
        cat("data_matrix is not all numeric... \n Coercing to numeric")
        char_cols <- sapply(data_matrix, is.character)
        data_matrix[, char_cols] <- lapply(data_matrix[, char_cols], 
            function(col) as.numeric(as.factor(col)))
    }
    #initialising local variables
    samples <- nrow(data_matrix)
    features <- ncol(data_matrix)
    iter <- 1
    
    stopifnot(k < samples)
    centroid_matrix <- data_matrix[sample(samples, k), ]

    SSE <- 0
    while(iter < max_iterations) { 
        distance_matrix <- distance(data_matrix, centroid_matrix, distfun)

        if (iter > 1) old_membership_vector <- membership_vector

        membership_vector <- compute_membership_vector(distance_matrix)

        old_centroid_matrix <- centroid_matrix
        centroid_matrix <- recompute_centroid_location(data_matrix, membership_vector)


        for (i in 1:k) {
            distance_moved <- fields::rdist.vec(old_centroid_matrix, centroid_matrix)
        }

        oldSSE <- SSE
        SSE <- 0
        #SSE calculation
        ## Garbage way of doing that
        for (i in 1:k) {
            centroid <- centroid_matrix[i, ]
            for (j in 1:sum(membership_vector %in% i)) {
                SSE <- SSE + distfun(data_matrix[j, ], centroid) ^ 2
            }
        }
        #oldSSE- SSE >


        #could change if structure to change escape 

        iter <- iter + 1
        if (iter >= 3) {
            if ((oldSSE - SSE) < 0.01) break
            else if (distance_moved < 0.01) break
            else if (identical(membership_vector, old_membership_vector))
                return(structure())
        }
    }
    structure(
        "membership_vector" = membership_vector,
        "iterations" = iter,
        "old membership vector" = old_membership_vector,
        totalsse = sse,
        betweensse=sse,
        class = "k_means")
}
