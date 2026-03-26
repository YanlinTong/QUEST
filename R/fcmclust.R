#' Fuzzy c-means clustering
#'
#' Perform fuzzy c-means clustering with multiple random initializations
#' and return the best solution based on the final objective value.
#'
#' @param x A numeric matrix of observations, with rows representing samples
#'   and columns representing features.
#' @param nclus Number of clusters.
#' @param iter.max Maximum number of iterations for each initialization.
#' @param verbose Logical; whether to print convergence messages.
#' @param m Fuzziness parameter. Must be greater than 1.
#' @param reltol Relative tolerance for convergence.
#' @param initialnum Number of random initializations.
#'
#' @return An object of class `"fcmclust"` containing:
#' \itemize{
#'   \item nclus: Number of clusters.
#'   \item m: Fuzziness parameter.
#'   \item initialnum: Number of random initializations.
#'   \item membership: Fuzzy membership matrix.
#'   \item cluster: Hard cluster labels.
#'   \item centers: Cluster centers.
#'   \item withinerror: Final objective value.
#'   \item size: Cluster sizes.
#'   \item iter: Number of iterations until convergence.
#'   \item call: Matched function call.
#' }
#' @export
fcmclust <- function(x,
                     nclus,
                     iter.max = 100,
                     verbose = TRUE,
                     m = 2,
                     reltol = 1e-4,
                     initialnum = 10) {
  x <- as.matrix(x)

  if (!is.numeric(x)) {
    stop("x must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (!is.numeric(nclus) || length(nclus) != 1 || nclus < 2) {
    stop("nclus must be a single integer greater than or equal to 2.")
  }
  if (!is.numeric(iter.max) || length(iter.max) != 1 || iter.max < 1) {
    stop("iter.max must be a single positive integer.")
  }
  if (!is.numeric(m) || length(m) != 1 || m <= 1) {
    stop("m must be a single numeric value greater than 1.")
  }
  if (!is.numeric(reltol) || length(reltol) != 1 || reltol <= 0) {
    stop("reltol must be a single positive numeric value.")
  }
  if (!is.numeric(initialnum) || length(initialnum) != 1 || initialnum < 1) {
    stop("initialnum must be a single positive integer.")
  }

  nclus <- as.integer(nclus)
  iter.max <- as.integer(iter.max)
  initialnum <- as.integer(initialnum)

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

  for (init in seq_len(initialnum)) {
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

      if (iter >= iter.max) {
        if (verbose) {
          message(sprintf("Reached maximum number of iterations: %d", iter.max))
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
    initialnum = initialnum,
    membership = best_u,
    cluster = as.character(best_cluster),
    centers = best_centers,
    withinerror = best_error,
    size = best_size,
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
  cat("Fuzzy c-means clustering with", x$nclus, "clusters\n")

  cat("\nm:\n")
  print(x$m, ...)

  cat("\nNumber of initializations:\n")
  print(x$initialnum, ...)

  cat("\nMemberships (first 10 data):\n")
  print(utils::head(x$membership, 10), ...)

  cat("\nClosest hard clustering (first 10 data):\n")
  print(x$cluster[1:min(10, length(x$cluster))], ...)

  cat("\nCluster centers:\n")
  print(x$centers, ...)

  cat("\nFinal error (within-cluster sum of squares):\n")
  cat(x$withinerror, "\n")

  cat("\nCluster sizes:\n")
  print(x$size, ...)

  cat("\nNumber of iterations until convergence:\n")
  cat(x$iter, "\n")

  invisible(x)
}
