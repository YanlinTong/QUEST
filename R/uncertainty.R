#' Compute uncertainty from membership matrix
#'
#' This function computes entropy-based uncertainty for each observation
#' using its membership vector and identifies uncertain locations based on
#' either a mean-plus-SD threshold or a quantile threshold.
#'
#' @param u A numeric matrix of dimension \eqn{n \times k}, where \eqn{n}
#'   is the number of observations and \eqn{k} is the number of clusters.
#'   Each row should represent a membership vector.
#' @param normalized A logical value indicating whether to return normalized entropy.
#' @param threshold_method A character string specifying the thresholding method.
#'   Must be one of `"mean_sd"` or `"quantile"`.
#' @param threshold_scale A numeric value specifying the multiplier of the
#'   standard deviation when `threshold_method = "mean_sd"`.
#' @param quantile_prob A numeric value between 0 and 1 specifying the quantile
#'   probability when `threshold_method = "quantile"`. Ignored otherwise.
#'
#' @return A list containing:
#' \itemize{
#'   \item entropy: A numeric vector of entropy values.
#'   \item threshold: The threshold for entropy used to define uncertainty.
#'   \item loc_uncertain: Indices of uncertain observations.
#'   \item is_uncertain: A logical vector indicating uncertain observations.
#'   \item threshold_method: The thresholding method used.
#'   \item threshold_scale: The SD multiplier used when `threshold_method = "mean_sd"`.
#'   \item quantile_prob: The quantile probability used when `threshold_method = "quantile"`,
#'     and `NULL` otherwise.
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

  if (!is.numeric(u)) {
    stop("u must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(u))) {
    stop("u must not contain missing values.")
  }
  if (ncol(u) < 2) {
    stop("u must have at least two columns.")
  }
  if (any(u < 0)) {
    stop("Membership values must be non-negative.")
  }

  if (!is.logical(normalized) || length(normalized) != 1 || is.na(normalized)) {
    stop("normalized must be a single logical value.")
  }

  if (!is.numeric(threshold_scale) || length(threshold_scale) != 1 || is.na(threshold_scale)) {
    stop("threshold_scale must be a single numeric value.")
  }

  row_sums <- rowSums(u)
  if (any(abs(row_sums - 1) > 1e-6)) {
    stop("Each row of u must sum to 1.")
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
  } else if (threshold_method == "quantile") {

    if (is.null(quantile_prob) ||
        !is.numeric(quantile_prob) ||
        length(quantile_prob) != 1 ||
        is.na(quantile_prob) ||
        quantile_prob < 0 || quantile_prob > 1) {

      stop("quantile_prob must be a single numeric value between 0 and 1 when threshold_method = 'quantile'.")
    }

    threshold <- as.numeric(
      stats::quantile(entropy, probs = quantile_prob, na.rm = TRUE)
    )
  }

  is_uncertain <- entropy > threshold

  list(
    entropy = entropy,
    threshold = threshold,
    loc_uncertain = which(is_uncertain),
    is_uncertain = is_uncertain,
    threshold_method = threshold_method,
    threshold_scale = threshold_scale,
    quantile_prob = quantile_prob,
    normalized = normalized
  )
}
