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


plot.clusterobject <- function(clusterobject, pch = 16, ...) {
    pca <- prcomp(clusterobject[["original_data_matrix"]])
    groups <- clusterobject[["membership_vector"]]
    

    # ggplot2::ggplot(data=data.frame(pca$x), aes(x=PC1, y=PC2)) + 
    #     ggplot2::geom_point(aes(colour = as.factor(groups))) +
    #     ggplot2::stat_ellipse(aes(colour = as.factor(groups))) +
    #     ggplot2::theme_bw() +
    #     ggplot2::ggtitle(paste0(class(clusterobject)[1], "PCA Plot")) +
    #     ggplot2::guides(colour=guide_legend(title="Clusters"))
    

    ## todo: title, labels, legend
    plot(pca$x, col = groups, ...)
    clusterobject

}


