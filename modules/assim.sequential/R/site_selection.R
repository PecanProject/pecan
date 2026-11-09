#' Perform Stratified Site Selection
#'
#' Selects sampling sites stratified by land cover class `lc`, scaling sample 
#' sizes proportionally while guaranteeing a minimum sample size per class. 
#' Optional normalization is applied prior to clustering.
#'
#' @param df Data frame or matrix containing feature columns, a land cover column `lc`, 
#'   and a weight column `weights`, where each row is a sample and each column is a feature.
#' @param tot.sample.size Integer. Total target number of sample points to select across all land cover classes.
#' @param min.sample.size Integer. Minimum number of sample points allocated to each land cover class. (Default = 50).
#' @param normalize Logical. Whether to apply min-max scaling to numeric columns in `df`. (Default = TRUE).
#' @param max.cluster.num Integer. Maximum number of clusters to consider if calculating optimal cluster count. (Default = 6).
#' @param elbow.operation Logical. Whether to automatically calculate the proper cluster number based on the elbow location. (Default = TRUE).
#' If FALSE, the `max.cluster.num` will be used directly as the cluster size.
#'
#' @return A list of cluster allocations and sample indices grouped by land cover class.
#' @export
site_selction <- function (df, tot.sample.size, min.sample.size = 50, normalize = TRUE, max.cluster.num = 6, elbow.operation = TRUE) {
  # grab column names from df.
  col.names <- colnames(df)
  # make sure we have both land cover (lc) and weights (weight) layer in the df object.
  if (!grepl("lc", col.names) & !grepl("weights", col.names)) {
    PEcAn.logger::logger.info("Please add land cover (lc) and weights (weight) columns to the df!")
    return(0)
  }
  # remove NAs if there is any.
  if (any(!complete.cases(df))) {
    non.na.inds <- which(stats::complete.cases(df))
    df <- df[non.na.inds, ]
  } else {
    non.na.inds <- NULL
  }
  # normalize df.
  if (normalize) {
    # Define min-max scaling function
    min_max_scale <- function(x) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }
    
    # Apply to all numeric columns in df
    df <- as.data.frame(lapply(df, function(col) {
      if (is.numeric(col)) min_max_scale(col) else col
    }))
  }
  # calculate sample size per each land cover class.
  # land cover operation.
  LC.inds <- sort(unique(df$lc)) %>% 
    purrr::map(function(i) {
      which(df$lc == (i))
    })
  proportion <- LC.inds %>% purrr::map(length) %>% unlist
  proportion <- proportion/sum(proportion)
  # TODO: account for the pre-selected locations during the site selection.
  tot.points.num <- smooth.div(tot.sample.size, (tot.sample.size - min.sample.size*length(LC.inds)) * proportion + min.sample.size)
  # loop over land cover classes.
  clusters <- vector("list", length = length(LC.inds))
  for (i in seq_along(clusters)) {
    # prepare datasets for the clustering of the current land cover class.
    temp.df <- df[LC.inds[[i]], ]
    temp.weights <- temp.df$weights
    # if we want to calculate the proper cluster size based on the elbow location of total_within_ss.
    if (elbow.operation) {
      total_within_ss <- c()
      for (j in seq_len(max.cluster.num)) {
        temp.kmeans <- faiss_kmeans(data_matrix = temp.df, k = j)
        total_within_ss <- c(total_within_ss, temp.kmeans$total_within_ss)
      }
      size <- pathviewr::find_curve_elbow(df)
    } else {
      size <- max.cluster.num # default cluster size.
    }
    # calculate sample size for each cluster.
    clust.sample.size <- smooth.div(tot.points.num[i],  rep(tot.points.num[i]/size, size))
    # conduct kmeans clustering with the selected cluster size.
    temp.kmeans <- faiss_kmeans(data_matrix = temp.df, k = size)
    # loop over clusters.
    temp.cluster.inds <- vector("list", length = size)
    for (j in seq_len(size)) {
      temp.inds <- which(temp.kmeans$cluster_ids == j)
      # weighted sampling.
      samples <- sample(x = temp.inds, size = clust.sample.size[j], 
                        replace = F, prob = temp.weights[temp.inds])
      # below we need to convert to the actual index.
      # if we observe NAs in the original data frame.
      if (!is.null(non.na.inds)) {
        temp.cluster.inds[[j]] <- non.na.inds[LC.inds[[i]][samples]]
      } else {
        temp.cluster.inds[[j]] <- LC.inds[[i]][samples]
      }
    }
    # store results.
    clusters[[i]] <- list(cluster.ids = temp.cluster.inds)
  }
  return(clusters)
}

#' Smoothly Distribute a Total Amount Across Vector Elements
#'
#' Distributes an integer total (`tot`) across the elements of a numeric vector (`v`). 
#' by alternating ceiling and floor operations to prevent rounding bias, while ensuring 
#' that the returned vector sums exactly to `tot`.
#'
#' @param tot Numeric/Integer. The target sum total to distribute.
#' @param v Numeric vector containing fractional values or proportion targets.
#'
#' @return A numeric vector of the same length as `v` containing integer values that sum to `tot`.
smooth.div <- function(tot, v) {
  vec <- c()
  ceil <- TRUE
  remain <- tot
  
  for (i in seq_along(v)) {
    if (i == length(v)) {
      vec <- c(vec, remain)
      break
    }
    if (ceil) {
      temp <- ceiling(v[i])
    } else {
      temp <- floor(v[i])
    }
    ceil <- !ceil
    remain <- remain - temp
    vec <- c(vec, temp)
  }
  vec
}