get_clusters <- function(clusterobject) {
    assert_that(class(clusterobject) == "clusterobject")
    membership_vector <- clusterobject[["membership_vector"]]

    data_matrix <- clusterobject[["data_matrix"]]

    lapply(sort(unique(membership_vector)), function(i) {
        data_matrix[membership_vector == i, ]
    })
}
