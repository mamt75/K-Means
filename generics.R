# print.k_means <- function(k_means) {
# }
# print.x_means <- function(x_means) {    
# }

print.clusterobject <- function(clusterobject) {
    cat("A ", class(clusterobject), 
        "object with ", clusterobject[["k"]], "clusters.\n\t",
        "Use \"get_clusters()\" to return a list of clusters,\n\t",
        "Or \"membership_vector()\" to return the members of each cluster."
        sep = "")
}
