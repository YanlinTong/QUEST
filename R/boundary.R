#' Detect boundary uncertain locations
#'
#' Identify boundary locations among uncertain observations based on spatial
#' nearest neighbors. An uncertain location is defined as a boundary location
#' if:
#' \itemize{
#'   \item it has at least one certain location among its \eqn{n_neighbors} nearest neighbors;
#'   \item its \eqn{n_neighbors} nearest neighbors contain at least two distinct cluster labels; and
#'   \item its own cluster label is represented among its \eqn{n_neighbors} nearest neighbors.
#' }
#'
#' @param dat_loc A numeric matrix of dimension \eqn{n \times d}, where \eqn{n}
#'   is the number of observations and \eqn{d} is the number of spatial coordinates.
#'   Each row corresponds to one observation.
#' @param cluster A vector of length \eqn{n} giving hard cluster labels.
#' @param is_uncertain A logical vector of length \eqn{n} indicating whether
#'   each observation is uncertain.
#' @param n_neighbors An integer specifying the number of nearest neighbors.
#'
#' @return A list containing:
#' \itemize{
#'   \item loc_boundary: Indices of boundary uncertain locations.
#'   \item is_boundary: A logical vector of length \eqn{n} indicating uncertain boundary status.
#'   \item loc_uncertain: Indices of uncertain locations.
#'   \item loc_certain: Indices of certain locations.
#'   \item n_boundary: Number of boundary uncertain locations.
#'   \item prop_boundary_in_uncertain: Proportion of boundary uncertain locations
#'     among uncertain locations.
#' }
#' @export
detect_boundary <- function(dat_loc, cluster, is_uncertain, n_neighbors) {
  dat_loc <- as.matrix(dat_loc)
  cluster <- as.vector(cluster)
  is_uncertain <- as.logical(is_uncertain)

  if (!is.numeric(dat_loc)) {
    stop("dat_loc must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(dat_loc))) {
    stop("dat_loc must not contain missing values.")
  }

  n <- nrow(dat_loc)

  if (length(cluster) != n) {
    stop("cluster must have length equal to nrow(dat_loc).")
  }
  if (any(is.na(cluster))) {
    stop("cluster must not contain missing values.")
  }

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

  loc_uncertain <- which(is_uncertain)
  loc_certain <- which(!is_uncertain)

  if (length(loc_uncertain) == 0) {
    return(list(
      loc_boundary = integer(0),
      is_boundary = rep(FALSE, n),
      loc_uncertain = integer(0),
      loc_certain = loc_certain,
      n_boundary = 0L,
      prop_boundary_in_uncertain = NA_real_
    ))
  }

  dat_loc_uncertain <- dat_loc[loc_uncertain, , drop = FALSE]

  nn <- FNN::get.knnx(
    data = dat_loc,
    query = dat_loc_uncertain,
    k = n_neighbors + 1
  )

  neighbor_idx <- nn$nn.index[, -1, drop = FALSE]

  is_boundary_uncertain <- vapply(seq_len(nrow(dat_loc_uncertain)), function(i) {
    neighbors <- neighbor_idx[i, ]
    self_idx <- loc_uncertain[i]
    self_label <- cluster[self_idx]
    neighbor_labels <- cluster[neighbors]

    condition1 <- any(neighbors %in% loc_certain)
    condition2 <- length(unique(neighbor_labels)) >= 2
    condition3 <- self_label %in% neighbor_labels

    condition1 && condition2 && condition3
  }, logical(1))

  is_boundary <- rep(FALSE, n)
  is_boundary[loc_uncertain] <- is_boundary_uncertain

  loc_boundary <- which(is_boundary)
  n_boundary <- length(loc_boundary)
  prop_boundary_in_uncertain <- n_boundary / length(loc_uncertain)

  list(
    loc_boundary = loc_boundary,
    is_boundary = is_boundary,
    loc_uncertain = loc_uncertain,
    loc_certain = loc_certain,
    n_boundary = n_boundary,
    prop_boundary_in_uncertain = prop_boundary_in_uncertain
  )
}
