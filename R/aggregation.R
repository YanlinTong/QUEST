#' Detect aggregated uncertain locations
#'
#' Identify aggregated locations among uncertain observations using a
#' three-step procedure:
#' \itemize{
#'   \item Step 1: Select uncertain locations with at least a specified number
#'     (or proportion) of uncertain neighbors among their \eqn{n\_neighbors}
#'     nearest neighbors.
#'   \item Step 2: Expand the aggregation set by adding uncertain neighbors of
#'     the Step 1 aggregation locations.
#'   \item Step 3: Build an igraph connectivity graph on the Step 2 aggregation
#'     locations and remove small connected components.
#' }
#'
#' Optionally, highly uncertain locations (for example, locations with entropy
#' greater than mean + 1.5 sd) can be forcibly added in Step 1.
#'
#' @param dat_loc A numeric matrix of dimension \eqn{n \times \rho}, where \eqn{n}
#'   is the number of observations and \eqn{\rho} is the spatial coordinate dimension
#'   (for example, \eqn{\rho = 2} for 2D coordinates).
#'   Each row corresponds to one observation.
#' @param is_uncertain A logical vector of length \eqn{n} indicating whether
#'   each observation is uncertain.
#' @param n_neighbors An integer specifying the number of nearest neighbors.
#' @param threshold_count An integer specifying the minimum number of uncertain
#'   neighbors required in Step 1. If provided, this takes precedence over
#'   `threshold_prop`.
#' @param threshold_prop A numeric value between 0 and 1 specifying the minimum
#'   proportion of uncertain neighbors required in Step 1. Used only when
#'   `threshold_count = NULL`.
#' @param add_high_uncertain_step1 A logical value indicating whether to forcibly
#'   add highly uncertain locations in Step 1.
#' @param high_uncertain Either a logical vector of length \eqn{n} or an integer
#'   vector of indices specifying highly uncertain locations to be added in Step 1
#'   when `add_high_uncertain_step1 = TRUE`.
#' @param do_step2 A logical value indicating whether to expand aggregation
#'   locations by adding uncertain neighbors of the Step 1 aggregation locations.
#' @param distance_threshold A positive numeric value specifying the distance
#'   threshold used to define graph connectivity in Step 3. If `NULL`, it is
#'   automatically set to 1.1 times the median nearest-neighbor distance.
#' @param min_component_size An integer specifying the minimum connected component
#'   size to keep in Step 3. If `NULL`, it is automatically set to
#'   `floor(0.003 * nrow(dat_loc))`.
#'
#' @return A list containing:
#' \itemize{
#'   \item loc_aggre: Indices of final aggregation locations.
#'   \item is_aggre: A logical vector of length \eqn{n} indicating uncertain aggregation status.
#'   \item aggre_cluster: An integer vector of length \eqn{n} giving reordered igraph
#'     component labels for final aggregation locations, and `NA` otherwise.
#'   \item loc_uncertain: Indices of uncertain locations.
#'   \item loc_certain: Indices of certain locations.
#'   \item n_aggre: Number of final aggregation locations.
#'   \item prop_aggre_in_uncertain: Proportion of final aggregation locations
#'     among uncertain locations.
#' }
#' @export
detect_aggregation <- function(
    dat_loc,
    is_uncertain,
    n_neighbors,
    threshold_count = NULL,
    threshold_prop = NULL,
    add_high_uncertain_step1 = FALSE,
    high_uncertain = NULL,
    do_step2 = TRUE,
    distance_threshold = NULL,
    min_component_size = NULL
) {
  dat_loc <- as.matrix(dat_loc)
  is_uncertain <- as.logical(is_uncertain)

  if (!is.numeric(dat_loc)) {
    stop("dat_loc must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(dat_loc))) {
    stop("dat_loc must not contain missing values.")
  }

  n <- nrow(dat_loc)

  if (length(is_uncertain) != n) {
    stop("is_uncertain must have length equal to nrow(dat_loc).")
  }
  if (any(is.na(is_uncertain))) {
    stop("is_uncertain must not contain missing values.")
  }

  if (!is.numeric(n_neighbors) || length(n_neighbors) != 1 || is.na(n_neighbors) ||
      n_neighbors <= 0 || n_neighbors != as.integer(n_neighbors)) {
    stop("n_neighbors must be a single positive integer.")
  }
  n_neighbors <- as.integer(n_neighbors)

  if (n_neighbors >= n) {
    stop("n_neighbors must be smaller than the total number of observations.")
  }

  if (!is.logical(add_high_uncertain_step1) || length(add_high_uncertain_step1) != 1 ||
      is.na(add_high_uncertain_step1)) {
    stop("add_high_uncertain_step1 must be a single non-missing logical value.")
  }

  if (!is.logical(do_step2) || length(do_step2) != 1 || is.na(do_step2)) {
    stop("do_step2 must be a single non-missing logical value.")
  }

  if (!is.null(threshold_count) && !is.null(threshold_prop)) {
    stop("Please provide only one of threshold_count or threshold_prop.")
  }

  if (!is.null(threshold_count)) {
    if (!is.numeric(threshold_count) || length(threshold_count) != 1 ||
        is.na(threshold_count) || threshold_count < 0 ||
        threshold_count > n_neighbors || threshold_count != as.integer(threshold_count)) {
      stop("threshold_count must be a single integer between 0 and n_neighbors.")
    }
    threshold_count <- as.integer(threshold_count)
  } else if (!is.null(threshold_prop)) {
    if (!is.numeric(threshold_prop) || length(threshold_prop) != 1 ||
        is.na(threshold_prop) || threshold_prop < 0 || threshold_prop > 1) {
      stop("threshold_prop must be a single numeric value between 0 and 1.")
    }
    threshold_count <- as.integer(floor(threshold_prop * n_neighbors))
  } else {
    threshold_count <- as.integer(floor(0.9 * n_neighbors))
  }

  if (is.null(distance_threshold) || !is.numeric(distance_threshold) ||
      length(distance_threshold) != 1 || is.na(distance_threshold) ||
      distance_threshold <= 0) {

    message("Invalid 'distance_threshold' provided. Using default: 1.1 × median nearest-neighbor distance.")

    nn_dist <- FNN::get.knn(dat_loc, k = 1)
    grid_spacing <- median(nn_dist$nn.dist[, 1])
    distance_threshold <- 1.1 * grid_spacing
  }

  if (is.null(min_component_size) || !is.numeric(min_component_size) ||
      length(min_component_size) != 1 || is.na(min_component_size) ||
      min_component_size < 1) {

    message("Invalid 'min_component_size' provided. Using default: floor(0.003 × n).")

    min_component_size <- max(1L, as.integer(floor(0.003 * nrow(dat_loc))))
  }

  if (min_component_size != as.integer(min_component_size)) {
    stop("min_component_size must be a single positive integer.")
  }
  min_component_size <- as.integer(min_component_size)

  loc_uncertain <- which(is_uncertain)
  loc_certain <- which(!is_uncertain)

  if (length(loc_uncertain) == 0) {
    return(list(
      loc_aggre = integer(0),
      is_aggre = rep(FALSE, n),
      aggre_cluster = rep(NA_integer_, n),
      loc_uncertain = integer(0),
      loc_certain = loc_certain,
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
    k = n_neighbors + 1
  )
  neighbor_idx1 <- nn1$nn.index[, -1, drop = FALSE]

  neighbor_uncertain_counts <- vapply(seq_len(nrow(neighbor_idx1)), function(i) {
    sum(neighbor_idx1[i, ] %in% loc_uncertain)
  }, integer(1))

  loc_aggre_step1 <- loc_uncertain[neighbor_uncertain_counts >= threshold_count]

  if (add_high_uncertain_step1) {
    if (is.null(high_uncertain)) {
      stop("Please provide high_uncertain when add_high_uncertain_step1 = TRUE.")
    }

    if (is.logical(high_uncertain)) {
      if (length(high_uncertain) != n) {
        stop("Logical high_uncertain must have length equal to nrow(dat_loc).")
      }
      if (any(is.na(high_uncertain))) {
        stop("Logical high_uncertain must not contain missing values.")
      }
      loc_high_uncertain <- which(high_uncertain)
    } else {
      if (any(is.na(high_uncertain))) {
        stop("high_uncertain must not contain missing values.")
      }
      loc_high_uncertain <- as.integer(high_uncertain)

      if (any(loc_high_uncertain < 1 | loc_high_uncertain > n)) {
        stop("Integer high_uncertain indices must be between 1 and nrow(dat_loc).")
      }
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
      k = n_neighbors + 1
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
    return(list(
      loc_aggre = integer(0),
      is_aggre = rep(FALSE, n),
      aggre_cluster = rep(NA_integer_, n),
      loc_uncertain = loc_uncertain,
      loc_certain = loc_certain,
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

  cluster_kept_raw <- cluster_raw[keep_flag]
  cluster_kept_reordered <- match(cluster_kept_raw, unique(cluster_kept_raw))

  is_aggre <- rep(FALSE, n)
  is_aggre[loc_aggre] <- TRUE

  aggre_cluster <- rep(NA_integer_, n)
  aggre_cluster[loc_aggre] <- cluster_kept_reordered

  n_aggre <- length(loc_aggre)
  prop_aggre_in_uncertain <- n_aggre / length(loc_uncertain)

  list(
    loc_aggre = loc_aggre,
    is_aggre = is_aggre,
    aggre_cluster = aggre_cluster,
    loc_uncertain = loc_uncertain,
    loc_certain = loc_certain,
    n_aggre = n_aggre,
    prop_aggre_in_uncertain = prop_aggre_in_uncertain
  )
}
