#' Compute uncertainty from membership matrix
#'
#' This function computes entropy-based uncertainty for each observation
#' using its membership vector and identifies uncertain locations based on
#' either a mean-plus-SD threshold or a quantile threshold.
#'
#' @param u A membership matrix (n x k).
#' @param normalized Logical; whether to return normalized entropy.
#' @param threshold_method Character; thresholding method. One of
#'   `"mean_sd"` or `"quantile"`.
#' @param threshold_scale Numeric; multiplier for SD in the mean-plus-SD
#'   threshold. Default is 0.5.
#' @param quantile_prob Numeric; quantile probability used when
#'   `threshold_method = "quantile"`. Must be between 0 and 1.
#'
#' @return A list with components:
#' \itemize{
#'   \item entropy: Numeric vector of entropy values.
#'   \item threshold: Threshold used to define uncertainty.
#'   \item is_uncertain: Logical vector indicating uncertain locations.
#'   \item threshold_method: Method used.
#'   \item normalized: Whether entropy was normalized.
#' }
#' @export
compute_uncertainty <- function(
    u,
    normalized = TRUE,
    threshold_method = c("mean_sd", "quantile"),
    threshold_scale = 0.5,
    quantile_prob = NULL
) {
  u <- as.matrix(u)

  if (any(u < 0)) {
    stop("Membership values must be non-negative.")
  }

  threshold_method <- match.arg(threshold_method)

  eps <- 1e-12
  u_safe <- pmax(u, eps)

  k <- ncol(u)

  entropy <- -rowSums(u_safe * log(u_safe))

  if (normalized) {
    entropy <- entropy / log(k)
  }

  if (threshold_method == "mean_sd") {
    threshold <- mean(entropy) + threshold_scale * stats::sd(entropy)
  }

  if (threshold_method == "quantile") {
    if (is.null(quantile_prob)) {
      stop("Please provide quantile_prob when threshold_method = 'quantile'.")
    }

    if (!is.numeric(quantile_prob) || length(quantile_prob) != 1 ||
        quantile_prob < 0 || quantile_prob > 1) {
      stop("quantile_prob must be between 0 and 1.")
    }

    threshold <- as.numeric(stats::quantile(entropy, probs = quantile_prob, na.rm = TRUE))
  }

  is_uncertain <- entropy > threshold

  list(
    entropy = entropy,
    threshold = threshold,
    is_uncertain = is_uncertain,
    threshold_method = threshold_method,
    normalized = normalized
  )
}
