#' Run the QUEST pipeline
#'
#' Run fuzzy c-means clustering, compute uncertainty, and optionally detect
#' boundary and aggregated uncertain locations. Optionally generate default
#' visualization outputs.
#'
#' @param x A numeric matrix of dimension \eqn{n \times d}, where \eqn{n}
#'   is the number of observations and \eqn{d} is the number of features.
#' @param dat_loc A numeric matrix of dimension \eqn{n \times \rho}, where
#'   \eqn{n} is the number of observations and \eqn{\rho} is the spatial
#'   coordinate dimension.
#' @param nclus An integer specifying the number of clusters.
#' @param auto_m A logical value indicating whether to automatically select the
#'   fuzziness parameter using `fcmclust_auto()`.
#' @param m A numeric value greater than 1 specifying the fuzziness parameter
#'   used when `auto_m = FALSE`.
#' @param vec_m A numeric vector specifying candidate fuzziness parameters used
#'   when `auto_m = TRUE`.
#' @param nstart An integer specifying the number of random initializations.
#' @param iter_max An integer specifying the maximum number of iterations for
#'   each initialization.
#' @param reltol A numeric value specifying the relative tolerance for convergence.
#' @param verbose A logical value indicating whether to print progress messages.
#' @param return_all_fits A logical value indicating whether to return all fitted
#'   models over candidate `m` values when `auto_m = TRUE`.
#' @param uncertainty_normalized A logical value indicating whether to normalize
#'   uncertainty.
#' @param uncertainty_threshold_method A character string specifying the threshold
#'   method for uncertainty. Must be one of `"mean_sd"` or `"quantile"`.
#' @param uncertainty_threshold_scale A numeric value specifying the SD multiplier
#'   used when `uncertainty_threshold_method = "mean_sd"`.
#' @param uncertainty_quantile_prob A numeric value between 0 and 1 specifying
#'   the quantile probability used when
#'   `uncertainty_threshold_method = "quantile"`.
#' @param run_boundary A logical value indicating whether to detect boundary
#'   uncertain locations.
#' @param boundary_n_neighbors An integer specifying the number of nearest
#'   neighbors for boundary detection.
#' @param run_aggregation A logical value indicating whether to detect aggregated
#'   uncertain locations.
#' @param aggregation_n_neighbors An integer specifying the number of nearest
#'   neighbors for aggregation detection.
#' @param aggregation_threshold_count An integer specifying the minimum number of
#'   uncertain neighbors required in Step 1 of aggregation detection.
#' @param aggregation_threshold_prop A numeric value between 0 and 1 specifying
#'   the minimum proportion of uncertain neighbors required in Step 1 of
#'   aggregation detection.
#' @param add_high_uncertain_step1 A logical value indicating whether to add
#'   extremely uncertain locations in Step 1 of aggregation detection.
#' @param high_uncertainty_threshold_method A character string specifying the
#'   threshold method for defining extremely uncertain locations used in aggregation.
#'   Must be one of `"mean_sd"` or `"quantile"`.
#' @param high_uncertainty_threshold_scale A numeric value specifying the SD
#'   multiplier used when `high_uncertainty_threshold_method = "mean_sd"`.
#' @param high_uncertainty_quantile_prob A numeric value between 0 and 1
#'   specifying the quantile probability used when
#'   `high_uncertainty_threshold_method = "quantile"`.
#' @param aggregation_do_step2 A logical value indicating whether to run Step 2
#'   of aggregation detection.
#' @param aggregation_distance_threshold A positive numeric value specifying the
#'   distance threshold used in Step 3 of aggregation detection. If `NULL`, it is
#'   automatically determined inside `detect_aggregation()`.
#' @param aggregation_min_component_size An integer specifying the minimum
#'   connected component size used in Step 3 of aggregation detection. If `NULL`,
#'   it is automatically determined inside `detect_aggregation()`.
#' @param make_plots A logical value indicating whether to generate default plots.
#'
#' @return A list containing:
#' \itemize{
#'   \item clustering: Result from `fcmclust()` or `fcmclust_auto()`.
#'   \item uncertainty: Result from `compute_uncertainty()`.
#'   \item boundary: Result from `detect_boundary()`, or `NULL`.
#'   \item aggregation: Result from `detect_aggregation()`, or `NULL`.
#'   \item plots: A list of default plots if `make_plots = TRUE`, otherwise `NULL`.
#'   \item settings: A list of key pipeline settings.
#'   \item call: Matched function call.
#' }
#' @export
quest <- function(
    x,
    dat_loc,
    nclus,
    auto_m = TRUE,
    m = 2,
    vec_m = seq(1.1, 2.5, by = 0.1),
    nstart = 10,
    iter_max = 100,
    reltol = 1e-4,
    verbose = FALSE,
    return_all_fits = FALSE,
    uncertainty_normalized = TRUE,
    uncertainty_threshold_method = c("mean_sd", "quantile"),
    uncertainty_threshold_scale = 0.5,
    uncertainty_quantile_prob = NULL,
    run_boundary = TRUE,
    boundary_n_neighbors = 10,
    run_aggregation = TRUE,
    aggregation_n_neighbors = 10,
    aggregation_threshold_count = NULL,
    aggregation_threshold_prop = NULL,
    add_high_uncertain_step1 = TRUE,
    high_uncertainty_threshold_method = c("mean_sd", "quantile"),
    high_uncertainty_threshold_scale = 1.5,
    high_uncertainty_quantile_prob = NULL,
    aggregation_do_step2 = TRUE,
    aggregation_distance_threshold = NULL,
    aggregation_min_component_size = NULL,
    make_plots = FALSE
) {
  x <- as.matrix(x)
  dat_loc <- as.matrix(dat_loc)
  uncertainty_threshold_method <- match.arg(uncertainty_threshold_method)
  high_uncertainty_threshold_method <- match.arg(high_uncertainty_threshold_method)

  if (!is.numeric(x)) {
    stop("x must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(x))) {
    stop("x must not contain missing values.")
  }
  if (!is.numeric(dat_loc)) {
    stop("dat_loc must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (any(is.na(dat_loc))) {
    stop("dat_loc must not contain missing values.")
  }
  if (nrow(x) != nrow(dat_loc)) {
    stop("x and dat_loc must have the same number of rows.")
  }
  if (!is.numeric(nclus) || length(nclus) != 1 || is.na(nclus) ||
      nclus < 2 || nclus != as.integer(nclus)) {
    stop("nclus must be a single integer greater than or equal to 2.")
  }
  if (!is.logical(auto_m) || length(auto_m) != 1 || is.na(auto_m)) {
    stop("auto_m must be a single non-missing logical value.")
  }
  if (!is.logical(run_boundary) || length(run_boundary) != 1 || is.na(run_boundary)) {
    stop("run_boundary must be a single non-missing logical value.")
  }
  if (!is.logical(run_aggregation) || length(run_aggregation) != 1 || is.na(run_aggregation)) {
    stop("run_aggregation must be a single non-missing logical value.")
  }
  if (!is.logical(make_plots) || length(make_plots) != 1 || is.na(make_plots)) {
    stop("make_plots must be a single non-missing logical value.")
  }

  nclus <- as.integer(nclus)

  ## Step 1: clustering
  if (auto_m) {
    clustering <- fcmclust_auto(
      x = x,
      nclus = nclus,
      vec_m = vec_m,
      nstart = nstart,
      iter_max = iter_max,
      reltol = reltol,
      verbose = verbose,
      return_all_fits = return_all_fits
    )
  } else {
    clustering <- fcmclust(
      x = x,
      nclus = nclus,
      m = m,
      nstart = nstart,
      iter_max = iter_max,
      reltol = reltol,
      verbose = verbose
    )
  }

  ## Step 2: uncertainty
  uncertainty <- compute_uncertainty(
    u = clustering$membership,
    normalized = uncertainty_normalized,
    threshold_method = uncertainty_threshold_method,
    threshold_scale = uncertainty_threshold_scale,
    quantile_prob = uncertainty_quantile_prob
  )

  ## Step 3: boundary
  boundary <- NULL
  if (run_boundary) {
    boundary <- detect_boundary(
      dat_loc = dat_loc,
      cluster = clustering$cluster,
      is_uncertain = uncertainty$is_uncertain,
      n_neighbors = boundary_n_neighbors
    )
  }

  ## Step 4: aggregation
  aggregation <- NULL
  high_uncertainty <- NULL

  if (run_aggregation) {
    high_uncertainty <- compute_uncertainty(
      u = clustering$membership,
      normalized = uncertainty_normalized,
      threshold_method = high_uncertainty_threshold_method,
      threshold_scale = high_uncertainty_threshold_scale,
      quantile_prob = high_uncertainty_quantile_prob
    )

    aggregation <- detect_aggregation(
      dat_loc = dat_loc,
      is_uncertain = uncertainty$is_uncertain,
      n_neighbors = aggregation_n_neighbors,
      threshold_count = aggregation_threshold_count,
      threshold_prop = aggregation_threshold_prop,
      add_high_uncertain_step1 = add_high_uncertain_step1,
      high_uncertain = high_uncertainty$is_uncertain,
      do_step2 = aggregation_do_step2,
      distance_threshold = aggregation_distance_threshold,
      min_component_size = aggregation_min_component_size
    )
  }

  ## Step 5: plots
  plots <- NULL
  if (make_plots) {
    plots <- list(
      clusters = plot_clusters(
        dat_loc = dat_loc,
        cluster = clustering$cluster,
        title = "Cluster"
      ),
      uncertainty_continuous = plot_uncertainty(
        dat_loc = dat_loc,
        uncertainty = uncertainty$entropy,
        mode = "continuous",
        title = "Uncertainty (continuous)"
      ),
      uncertainty_category = plot_uncertainty(
        dat_loc = dat_loc,
        uncertainty = uncertainty$entropy,
        mode = "category",
        title = "Uncertainty (category)"
      )
    )

    if (!is.null(boundary)) {
      plots$boundary <- plot_boundary(
        dat_loc = dat_loc,
        cluster = clustering$cluster,
        is_uncertain = uncertainty$is_uncertain,
        is_boundary = boundary$is_boundary,
        title = "Boundary",
        color_mode = "status"
      )
    }

    if (!is.null(aggregation)) {
      plots$aggregation <- plot_aggregation(
        dat_loc = dat_loc,
        cluster = clustering$cluster,
        is_uncertain = uncertainty$is_uncertain,
        is_aggre = aggregation$is_aggre,
        igraph_cluster = aggregation$aggre_cluster,
        title = "Aggregation",
        color_mode = "status"
      )
    }
  }

  settings <- list(
    auto_m = auto_m,
    m = if (auto_m) NULL else m,
    vec_m = if (auto_m) vec_m else NULL,
    nclus = nclus,
    nstart = nstart,
    iter_max = iter_max,
    reltol = reltol,
    verbose = verbose,
    return_all_fits = return_all_fits,
    uncertainty_normalized = uncertainty_normalized,
    uncertainty_threshold_method = uncertainty_threshold_method,
    uncertainty_threshold_scale = uncertainty_threshold_scale,
    uncertainty_quantile_prob = uncertainty_quantile_prob,
    run_boundary = run_boundary,
    boundary_n_neighbors = boundary_n_neighbors,
    run_aggregation = run_aggregation,
    aggregation_n_neighbors = aggregation_n_neighbors,
    aggregation_threshold_count = aggregation_threshold_count,
    aggregation_threshold_prop = aggregation_threshold_prop,
    add_high_uncertain_step1 = add_high_uncertain_step1,
    high_uncertainty_threshold_method = high_uncertainty_threshold_method,
    high_uncertainty_threshold_scale = high_uncertainty_threshold_scale,
    high_uncertainty_quantile_prob = high_uncertainty_quantile_prob,
    aggregation_do_step2 = aggregation_do_step2,
    aggregation_distance_threshold = aggregation_distance_threshold,
    aggregation_min_component_size = aggregation_min_component_size,
    make_plots = make_plots
  )

  list(
    clustering = clustering,
    uncertainty = uncertainty,
    #high_uncertainty = high_uncertainty,
    boundary = boundary,
    aggregation = aggregation,
    plots = plots,
    settings = settings,
    call = match.call()
  )
}
