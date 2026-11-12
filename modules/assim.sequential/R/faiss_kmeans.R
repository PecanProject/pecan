#' Perform K-means clustering using the FAISS Python library
#'
#' @param data_matrix Numeric matrix: Input dataset where each row is a sample and each column is a feature.
#' @param k Integer: The desired number of clusters (centroids).
#' @param niter Integer: Number of K-means training iterations (default: 20).
#' @param verbose Logical: Whether to display clustering progress and loss details during training (default: TRUE).
#' @param n_cores Integer: Number of CPU threads to use for execution (default: number of available cores minus 1).
#'
#' @return A FAISS K-means object containing cluster centroids and assignment details.
#' @export
#'
#' @examples
#' \dontrun{
#'   mat <- matrix(runif(1000 * 10), nrow = 1000, ncol = 10)
#'   res <- faiss_kmeans(data_matrix = mat, k = 5, niter = 25)
#' }
faiss_kmeans <- function(data_matrix, k, niter = 20, verbose = TRUE, n_cores = parallel::detectCores() - 1) {
  # Make sure we have the faiss package before using it.
  if (!reticulate::py_module_available("faiss")) {
    PEcAn.logger::logger.info("Python library faiss is not installed!")
    return(0)
  }
  
  # Import faiss object.
  faiss <- reticulate::import("faiss")
  # Set OpenMP threads for FAISS CPU execution
  faiss$omp_set_num_threads(as.integer(n_cores))
  
  # Reformat data to meet FAISS requirements
  data_mat <- as.matrix(data_matrix)
  mode(data_mat) <- "single"
  d <- ncol(data_mat)
  
  # Initialize FAISS K-means object
  kmeans <- faiss$Kmeans(
    d       = as.integer(d),
    k       = as.integer(k),
    niter   = as.integer(niter),
    verbose = verbose
  )
  
  # Train K-means on matrix
  kmeans$train(data_mat)
  
  # Compute cluster assignments.
  index <- faiss$IndexFlatL2(as.integer(d))
  index$add(kmeans$centroids)
  
  # Search nearest centroid for each point (explicit integer for k)
  res <- index$search(data_mat, 1L)
  
  # Total within-cluster variance (Total WCSS)
  total_within_ss <- sum(as.vector(res[[1]]))
  
  list(
    centroids = kmeans$centroids,
    cluster_ids = as.vector(res[[2]]) + 1, # Convert 0-indexed Python to 1-indexed R
    distances = as.vector(res[[1]]),
    total_within_ss = total_within_ss
  )
}