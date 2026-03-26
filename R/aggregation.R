#' Detect aggregated uncertain locations
#'
#' Identify aggregation locations among uncertain observations using a
#' three-step procedure:
#' \itemize{
#'   \item Step 1: Select uncertain locations with at least a specified number
#'     (or proportion) of uncertain neighbors among their k nearest neighbors.
#'   \item Step 2: Expand the aggregation set by adding uncertain neighbors of
#'     the Step 1 aggregation locations.
#'   \item Step 3: Build an igraph connectivity graph on the Step 2 aggregation
#'     locations and remove small connected components.
#' }
#'
#' Optionally, highly uncertain locations (for example, locations with entropy
#' greater than mean + 1.5 sd) can be forcibly added in Step 1.
#'
#' @param dat_loc A numeric matrix or data frame of spatial coordinates
#'   (n x d), where rows correspond to observations.
#' @param is_uncertain A logical vector of length n indicating whether each
#'   observation is uncertain.
#' @param k Integer; number of nearest neighbors to use.
#' @param threshold_count Integer; minimum number of uncertain neighbors among
#'   the k nearest neighbors required in Step 1. If provided, this takes
#'   precedence over `threshold_prop`.
#' @param threshold_prop Numeric between 0 and 1; minimum proportion of uncertain
#'   neighbors among the k nearest neighbors required in Step 1. Used only when
#'   `threshold_count = NULL`.
#' @param add_high_uncertain_step1 Logical; whether to forcibly add highly
#'   uncertain locations in Step 1.
#' @param high_uncertain Either a logical vector of length n or an integer vector
#'   of indices specifying highly uncertain locations to be added in Step 1 when
#'   `add_high_uncertain_step1 = TRUE`.
#' @param do_step2 Logical; whether to expand aggregation locations by adding
#'   uncertain neighbors of the Step 1 aggregation locations.
#' @param distance_threshold Numeric; distance threshold used to define graph
#'   connectivity in Step 3.
#' @param min_component_size Integer; minimum connected component size to keep
#'   in Step 3.
#'
#' @return A list containing:
#' \itemize{
#'   \item loc_aggre: Indices of final aggregation locations.
#'   \item is_aggregated: Logical vector of length n indicating aggregation status.
#'   \item igraph_cluster: Integer vector of length n giving reordered igraph
#'     component labels for final aggregation locations, and `NA` otherwise.
#'   \item igraph_cluster_aggre: Reordered igraph component labels only for final
#'     aggregation locations.
#'   \item loc_uncertain: Indices of uncertain locations.
#'   \item loc_certain: Indices of certain locations.
#'   \item loc_aggre_step1: Aggregation indices after Step 1.
#'   \item loc_aggre_step2: Aggregation indices after Step 2.
#'   \item neighbor_uncertain_counts: Number of uncertain neighbors for each
#'     uncertain location in Step 1.
#'   \item threshold_count: Final count threshold used in Step 1.
#'   \item n_aggre: Number of final aggregation locations.
#'   \item prop_aggre_in_uncertain: Proportion of final aggregation locations
#'     among uncertain locations.
#' }
#' @export
detect_aggregation <- function(
    dat_loc,
    is_uncertain,
    k,
    threshold_count = NULL,
    threshold_prop = NULL,
    add_high_uncertain_step1 = FALSE,
    high_uncertain = NULL,
    do_step2 = TRUE,
    distance_threshold,
    min_component_size = 300
) {
  dat_loc <- as.matrix(dat_loc)
  is_uncertain <- as.logical(is_uncertain)

  n <- nrow(dat_loc)

  if (length(is_uncertain) != n) {
    stop("is_uncertain must have length equal to nrow(dat_loc).")
  }
  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("k must be a positive integer.")
  }
  k <- as.integer(k)

  if (k >= n) {
    stop("k must be smaller than the total number of observations.")
  }

  if (!is.null(threshold_count) && !is.null(threshold_prop)) {
    stop("Please provide only one of threshold_count or threshold_prop.")
  }

  ## determine threshold_count with priority:
  ## 1. user-supplied threshold_count
  ## 2. user-supplied threshold_prop
  ## 3. default floor(0.9 * k)
  if (!is.null(threshold_count)) {
    if (!is.numeric(threshold_count) || length(threshold_count) != 1 ||
        threshold_count < 0 || threshold_count > k) {
      stop("threshold_count must be a single integer between 0 and k.")
    }
    threshold_count <- as.integer(threshold_count)
  } else if (!is.null(threshold_prop)) {
    if (!is.numeric(threshold_prop) || length(threshold_prop) != 1 ||
        threshold_prop < 0 || threshold_prop > 1) {
      stop("threshold_prop must be a single numeric value between 0 and 1.")
    }
    threshold_count <- as.integer(ceiling(threshold_prop * k))
  } else {
    threshold_count <- as.integer(floor(0.9 * k))
  }

  if (!is.numeric(distance_threshold) || length(distance_threshold) != 1 ||
      distance_threshold <= 0) {
    stop("distance_threshold must be a positive numeric value.")
  }

  if (!is.numeric(min_component_size) || length(min_component_size) != 1 ||
      min_component_size < 1) {
    stop("min_component_size must be a positive integer.")
  }
  min_component_size <- as.integer(min_component_size)

  loc_uncertain <- which(is_uncertain)
  loc_certain <- which(!is_uncertain)

  if (length(loc_uncertain) == 0) {
    return(list(
      loc_aggre = integer(0),
      is_aggregated = rep(FALSE, n),
      igraph_cluster = rep(NA_integer_, n),
      igraph_cluster_aggre = integer(0),
      loc_uncertain = integer(0),
      loc_certain = loc_certain,
      loc_aggre_step1 = integer(0),
      loc_aggre_step2 = integer(0),
      neighbor_uncertain_counts = integer(0),
      threshold_count = threshold_count,
      n_aggre = 0L,
      prop_aggre_in_uncertain = NA_real_
    ))
  }

  dat_loc_uncertain <- dat_loc[loc_uncertain, , drop = FALSE]

  ## -----------------------------
  ## Step 1
  ## -----------------------------
  nn1 <- FNN::get.knnx(
    data = dat_loc,
    query = dat_loc_uncertain,
    k = k + 1
  )
  neighbor_idx1 <- nn1$nn.index[, -1, drop = FALSE]

  neighbor_uncertain_counts <- apply(neighbor_idx1, 1, function(idx) {
    sum(idx %in% loc_uncertain)
  })

  loc_aggre_step1 <- loc_uncertain[neighbor_uncertain_counts >= threshold_count]

  if (add_high_uncertain_step1) {
    if (is.null(high_uncertain)) {
      stop("Please provide high_uncertain when add_high_uncertain_step1 = TRUE.")
    }

    if (is.logical(high_uncertain)) {
      if (length(high_uncertain) != n) {
        stop("Logical high_uncertain must have length equal to nrow(dat_loc).")
      }
      loc_high_uncertain <- which(high_uncertain)
    } else {
      loc_high_uncertain <- as.integer(high_uncertain)
    }

    loc_aggre_step1 <- sort(unique(c(loc_aggre_step1, loc_high_uncertain)))
  }

  ## -----------------------------
  ## Step 2
  ## -----------------------------
  if (do_step2 && length(loc_aggre_step1) > 0) {
    nn2 <- FNN::get.knnx(
      data = dat_loc,
      query = dat_loc[loc_aggre_step1, , drop = FALSE],
      k = k + 1
    )
    neighbor_idx2 <- nn2$nn.index[, -1, drop = FALSE]
    nearest_neighbors <- unique(as.vector(neighbor_idx2))
    aggre_add <- nearest_neighbors[nearest_neighbors %in% loc_uncertain]
    loc_aggre_step2 <- sort(unique(c(loc_aggre_step1, aggre_add)))
  } else {
    loc_aggre_step2 <- loc_aggre_step1
  }

  ## -----------------------------
  ## Step 3
  ## -----------------------------
  if (length(loc_aggre_step2) == 0) {
    is_aggregated <- rep(FALSE, n)
    igraph_cluster <- rep(NA_integer_, n)

    return(list(
      loc_aggre = integer(0),
      is_aggregated = is_aggregated,
      igraph_cluster = igraph_cluster,
      igraph_cluster_aggre = integer(0),
      loc_uncertain = loc_uncertain,
      loc_certain = loc_certain,
      loc_aggre_step1 = loc_aggre_step1,
      loc_aggre_step2 = loc_aggre_step2,
      neighbor_uncertain_counts = neighbor_uncertain_counts,
      threshold_count = threshold_count,
      n_aggre = 0L,
      prop_aggre_in_uncertain = 0
    ))
  }

  dat_aggre2 <- as.data.frame(dat_loc[loc_aggre_step2, , drop = FALSE])

  dist_matrix <- as.matrix(stats::dist(dat_aggre2))
  adj_matrix <- dist_matrix < distance_threshold

  graph <- igraph::graph_from_adjacency_matrix(
    adj_matrix,
    mode = "undirected",
    diag = FALSE
  )

  comp <- igraph::components(graph)
  cluster_raw <- comp$membership
  cluster_sizes <- table(cluster_raw)

  small_clusters <- names(cluster_sizes[cluster_sizes < min_component_size])
  keep_flag <- !(cluster_raw %in% small_clusters)

  loc_aggre <- loc_aggre_step2[keep_flag]

  ## reorder cluster labels after removing small components
  cluster_kept_raw <- cluster_raw[keep_flag]
  cluster_kept_reordered <- match(cluster_kept_raw, unique(cluster_kept_raw))

  is_aggregated <- rep(FALSE, n)
  is_aggregated[loc_aggre] <- TRUE

  igraph_cluster <- rep(NA_integer_, n)
  igraph_cluster[loc_aggre] <- cluster_kept_reordered

  n_aggre <- length(loc_aggre)
  prop_aggre_in_uncertain <- n_aggre / length(loc_uncertain)

  list(
    loc_aggre = loc_aggre,
    is_aggregated = is_aggregated,
    igraph_cluster = igraph_cluster,
    igraph_cluster_aggre = cluster_kept_reordered,
    loc_uncertain = loc_uncertain,
    loc_certain = loc_certain,
    loc_aggre_step1 = loc_aggre_step1,
    loc_aggre_step2 = loc_aggre_step2,
    neighbor_uncertain_counts = neighbor_uncertain_counts,
    threshold_count = threshold_count,
    n_aggre = n_aggre,
    prop_aggre_in_uncertain = prop_aggre_in_uncertain
  )
}
