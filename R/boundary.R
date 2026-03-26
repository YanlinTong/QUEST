#' Detect boundary uncertain locations
#'
#' Identify boundary locations among uncertain observations based on spatial
#' nearest neighbors. An uncertain location is defined as a boundary location
#' if:
#' \itemize{
#'   \item it has at least one certain location among its k nearest neighbors;
#'   \item its k nearest neighbors contain at least two distinct cluster labels; and
#'   \item its own cluster label is represented among its k nearest neighbors.
#' }
#'
#' @param dat_loc A numeric matrix or data frame of spatial coordinates
#'   (n x d), where rows correspond to observations.
#' @param cluster A vector of hard cluster labels of length n.
#' @param is_uncertain A logical vector of length n indicating whether each
#'   observation is uncertain.
#' @param k Integer; number of nearest neighbors to use.
#'
#' @return A list containing:
#' \itemize{
#'   \item loc_boundary: Indices of boundary uncertain locations.
#'   \item is_boundary: Logical vector of length equal to the number of uncertain
#'     locations, indicating whether each uncertain location is a boundary.
#'   \item loc_uncertain: Indices of uncertain locations.
#'   \item loc_certain: Indices of certain locations.
#'   \item n_boundary: Number of boundary uncertain locations.
#'   \item prop_boundary_in_uncertain: Proportion of boundary uncertain locations
#'     among uncertain locations.
#' }
#' @export
detect_boundary <- function(dat_loc, cluster, is_uncertain, k) {
  dat_loc <- as.matrix(dat_loc)
  cluster <- as.vector(cluster)
  is_uncertain <- as.logical(is_uncertain)

  n <- nrow(dat_loc)

  if (length(cluster) != n) {
    stop("cluster must have length equal to nrow(dat_loc).")
  }
  if (length(is_uncertain) != n) {
    stop("is_uncertain must have length equal to nrow(dat_loc).")
  }
  if (!is.numeric(k) || length(k) != 1 || k <= 0) {
    stop("k must be a positive integer.")
  }
  k <- as.integer(k)

  loc_uncertain <- which(is_uncertain)
  loc_certain <- which(!is_uncertain)

  if (length(loc_uncertain) == 0) {
    return(list(
      loc_boundary = integer(0),
      is_boundary = logical(0),
      loc_uncertain = integer(0),
      loc_certain = loc_certain,
      n_boundary = 0L,
      prop_boundary_in_uncertain = NA_real_
    ))
  }

  if (k >= n) {
    stop("k must be smaller than the total number of observations.")
  }

  dat_loc_uncertain <- dat_loc[loc_uncertain, , drop = FALSE]

  nn <- FNN::get.knnx(
    data = dat_loc,
    query = dat_loc_uncertain,
    k = k + 1
  )

  neighbor_idx <- nn$nn.index[, -1, drop = FALSE]

  is_boundary <- vapply(seq_len(nrow(dat_loc_uncertain)), function(i) {
    neighbors <- neighbor_idx[i, ]
    self_idx <- loc_uncertain[i]
    self_label <- cluster[self_idx]
    neighbor_labels <- cluster[neighbors]

    condition1 <- any(neighbors %in% loc_certain)
    condition2 <- length(unique(neighbor_labels)) >= 2
    condition3 <- self_label %in% neighbor_labels

    condition1 && condition2 && condition3
  }, logical(1))

  loc_boundary <- loc_uncertain[is_boundary]
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
