# Task 1
smart_hclust <- function(df, n_clusters) {
  dist_matrix <- dist(df)
  model <- hclust(dist_matrix)
  clusters <- cutree(model, n_clusters)
  df['cluster'] = as.factor(clusters)
  
  df
}

# Task 2
get_difference <- function(df, n_clusters) {
  vars = colnames(df)
  df = smart_hclust(df, n_clusters)
  p_vals = sapply(df[vars], function(c) summary(aov(c ~ df[, 'cluster']))[[1]]$'Pr(>F)'[1])
  vars[p_vals < 0.05]
}

# Task 3
get_pc <- function(df) {
  res = prcomp(df)
  
  df$PC1 = res$x[,'PC1']
  df$PC2 = res$x[,'PC2']

  df
}

# Task 4
get_pca2 <- function(df) {
  fit = prcomp(df)
  n_components = which(summary(fit)$importance['Cumulative Proportion',] > 0.9)[1]
  df = cbind(df, fit$x[, 1:n_components])
  df
}

# Task 5
is_multicol <- function(df) {
  positions <- subset(as.data.frame(which(abs(cor(df)) > 0.9999, arr.ind=TRUE)), row < col)
  
  if (nrow(positions) > 0) {
    as.vector(sapply(positions, function(x) names(df)[x]))
  } else {
    "There is no collinearity in the data"
  }
}

# Task 6
swiss = smart_hclust(swiss, 2)
my_plot = ggplot(swiss, aes(Education, Catholic, col = cluster)) + geom_smooth() + geom_point()
