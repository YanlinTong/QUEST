#' Fuzzy c-means clustering
#'
#' Perform fuzzy c-means clustering with multiple random initializations
#' and return the best solution based on the final objective value.
#'
#' @param x A numeric matrix of dimension \eqn{n \times d}, where \eqn{n}
#'   is the number of observations and \eqn{d} is the number of features.
#' @param nclus An integer specifying the number of clusters.
#' @param m A numeric value greater than 1 specifying the fuzziness parameter.
#' @param nstart An integer specifying the number of random initializations.
#' @param iter_max An integer specifying the maximum number of iterations for each initialization.
#' @param reltol A numeric value specifying the relative tolerance for convergence.
#' @param verbose A logical value indicating whether to print convergence messages.
#'
#' @return An object of class `"fcmclust"` containing:
#' \itemize{
#'   \item nclus: Number of clusters.
#'   \item m: Fuzziness parameter.
#'   \item nstart: Number of random initializations.
#'   \item membership: Fuzzy membership matrix.
#'   \item cluster: Hard cluster labels.
#'   \item size: Cluster sizes.
#'   \item centers: Cluster centers.
#'   \item withinerror: Final objective value.
#'   \item iter: Number of iterations until convergence.
#'   \item call: Matched function call.
#' }
#' @export
fcmclust <- function(x,
                     nclus,
                     m = 2,
                     nstart = 10,
                     iter_max = 100,
                     reltol = 1e-4,
                     verbose = FALSE) {
  x <- as.matrix(x)

  if (!is.numeric(x)) {
    stop("x must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(x))) {
    stop("x must not contain missing values.")
  }
  if (!is.numeric(nclus) || length(nclus) != 1 || is.na(nclus) ||
      nclus < 2 || nclus != as.integer(nclus)) {
    stop("nclus must be a single integer greater than or equal to 2.")
  }
  if (!is.numeric(m) || length(m) != 1 || is.na(m) || m <= 1) {
    stop("m must be a single numeric value greater than 1.")
  }
  if (!is.numeric(nstart) || length(nstart) != 1 || is.na(nstart) ||
      nstart < 1 || nstart != as.integer(nstart)) {
    stop("nstart must be a single positive integer.")
  }
  if (!is.numeric(iter_max) || length(iter_max) != 1 || is.na(iter_max) ||
      iter_max < 1 || iter_max != as.integer(iter_max)) {
    stop("iter_max must be a single positive integer.")
  }
  if (!is.numeric(reltol) || length(reltol) != 1 || is.na(reltol) || reltol <= 0) {
    stop("reltol must be a single positive numeric value.")
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("verbose must be a single logical value.")
  }

  nclus <- as.integer(nclus)
  nstart <- as.integer(nstart)
  iter_max <- as.integer(iter_max)

  xrows <- nrow(x)

  if (nclus > xrows) {
    stop("nclus cannot be greater than the number of observations.")
  }

  best_error <- Inf
  best_centers <- NULL
  best_u <- NULL
  best_iter <- 0L
  best_cluster <- NULL
  best_size <- NULL

  for (init in seq_len(nstart)) {
    centers <- x[sample(seq_len(xrows), nclus), , drop = FALSE]

    if (any(duplicated(centers))) {
      unique_x <- unique(x)
      unique_x_rows <- nrow(unique_x)

      if (unique_x_rows < nclus) {
        stop("More cluster centers than distinct data points.")
      }

      centers <- unique_x[sample(seq_len(unique_x_rows), nclus), , drop = FALSE]
    }

    iter <- 0L
    old_error <- Inf
    u <- matrix(0, nrow = xrows, ncol = nclus)

    repeat {
      distance <- calculate_dissimilarities_cpp(x, centers)
      u <- update_memberships_cpp(distance, m)
      centers <- update_prototypes_cpp(x, u, m)
      rownames(centers) <- seq_len(nclus)

      new_error <- calculate_error_cpp(u, distance, m)

      if (abs(new_error - old_error) < reltol * (old_error + reltol)) {
        if (verbose) {
          message(sprintf("Converged at iteration %d with error %f", iter, new_error))
        }
        break
      }

      old_error <- new_error
      iter <- iter + 1L

      if (iter >= iter_max) {
        if (verbose) {
          message(sprintf("Reached maximum number of iterations: %d", iter_max))
        }
        break
      }
    }

    cluster <- apply(u, 1, which.max)
    clustersize <- tabulate(cluster, nbins = nclus)

    if (new_error < best_error) {
      best_error <- new_error
      best_centers <- centers
      best_u <- u
      best_iter <- iter
      best_cluster <- cluster
      best_size <- clustersize
    }
  }


  retval <- list(
    nclus = nclus,
    m = m,
    nstart = nstart,

    membership = best_u,
    cluster = as.character(best_cluster),
    size = best_size,
    centers = best_centers,

    withinerror = best_error,

    iter = best_iter,
    call = match.call()
  )

  class(retval) <- "fcmclust"
  retval
}

#' Print an fcmclust object
#'
#' @param x An object of class `"fcmclust"`.
#' @param ... Additional arguments passed to `print()`.
#'
#' @return The input object, invisibly.
#' @export
print.fcmclust <- function(x, ...) {
  cat("Fuzzy c-means clustering with", x$nclus,
      "clusters and", x$nstart, "initializations\n")

  cat("\nFuzziness parameter (m):\n")
  print(x$m, ...)

  cat("\nMemberships (first 10 data):\n")
  print(utils::head(x$membership, 10), ...)

  cat("\nHard cluster labels (first 10 data):\n")
  print(x$cluster[1:min(10, length(x$cluster))], ...)

  cat("\nCluster sizes:\n")
  print(x$size, ...)

  cat("\nCluster centers:\n")
  print(x$centers, ...)

  cat("\nFinal objective value:\n")
  cat(x$withinerror, "\n")

  cat("\nNumber of iterations until convergence:\n")
  cat(x$iter, "\n")

  invisible(x)
}
