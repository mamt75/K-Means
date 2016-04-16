pick_centroids <- function(samples, k) {
    stopifnot(k < samples)
    sample(samples, k)
}

## useless function
create_centroid_matrix <- function(centroids, data_matrix) {
    centroid_matrix <- data_matrix[, centroids]
}



## Re-do (vectorise)
recompute_centroid_location <- function(data_matrix, membership_vector) {
    k <- unique(membership_vector)
    out <- matrix(
        data=NA, 
        nrow = nrow(data_matrix), 
        ncol = length(k)
    )
    for (i in k) {    
        out[, i] <- rowMeans(data_matrix[, membership_vector == i])
    }
    out
}
