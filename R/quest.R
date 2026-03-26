#' Run the QUEST pipeline
#'
#' Run fuzzy c-means clustering, compute uncertainty, and optionally detect
#' boundary and aggregated uncertain locations. Optionally generate default
#' visualization outputs.
#'
#' @param x A numeric matrix of observations.
#' @param dat_loc A numeric matrix or data frame of spatial coordinates
#'   with rows corresponding to observations.
#' @param nclus Number of clusters.
#' @param auto_m Logical; whether to automatically select the fuzziness
#'   parameter using `fcmclust_auto()`.
#' @param m Fuzziness parameter used when `auto_m = FALSE`.
#' @param vec_m Candidate fuzziness parameters used when `auto_m = TRUE`.
#' @param iter.max Maximum number of iterations for each initialization.
#' @param verbose Logical; whether to print progress.
#' @param reltol Relative tolerance for convergence.
#' @param initialnum Number of random initializations.
#' @param return_all_fits Logical; whether to return all fitted models over
#'   candidate `m` values when `auto_m = TRUE`.
#' @param uncertainty_normalized Logical; whether to normalize uncertainty.
#' @param uncertainty_threshold_method Character; one of `"mean_sd"` or
#'   `"quantile"`.
#' @param uncertainty_threshold_scale Numeric; standard deviation multiplier
#'   used when `uncertainty_threshold_method = "mean_sd"`.
#' @param uncertainty_quantile_prob Numeric; quantile probability used when
#'   `uncertainty_threshold_method = "quantile"`.
#' @param run_boundary Logical; whether to detect boundary uncertain locations.
#' @param boundary_k Integer; number of nearest neighbors for boundary detection.
#' @param run_aggregation Logical; whether to detect aggregated uncertain locations.
#' @param aggregation_k Integer; number of nearest neighbors for aggregation detection.
#' @param aggregation_threshold_count Integer; minimum number of uncertain
#'   neighbors required in Step 1 of aggregation detection. If `NULL`,
#'   `aggregation_threshold_prop` must be provided.
#' @param aggregation_threshold_prop Numeric between 0 and 1; minimum proportion
#'   of uncertain neighbors required in Step 1 of aggregation detection.
#' @param add_high_uncertain_step1 Logical; whether to add highly uncertain
#'   locations in Step 1 of aggregation detection.
#' @param high_uncertainty_threshold_method Character; one of `"mean_sd"` or
#'   `"quantile"` for defining highly uncertain locations used in aggregation.
#' @param high_uncertainty_threshold_scale Numeric; standard deviation multiplier
#'   used when `high_uncertainty_threshold_method = "mean_sd"`.
#' @param high_uncertainty_quantile_prob Numeric; quantile probability used when
#'   `high_uncertainty_threshold_method = "quantile"`.
#' @param aggregation_do_step2 Logical; whether to run Step 2 of aggregation detection.
#' @param aggregation_distance_threshold Numeric; distance threshold used in
#'   Step 3 of aggregation detection.
#' @param aggregation_min_component_size Integer; minimum connected component size
#'   to keep in Step 3 of aggregation detection.
#' @param plot Logical; whether to generate default plots.
#'
#' @return A list with components:
#' \itemize{
#'   \item clustering: Result from `fcmclust()` or `fcmclust_auto()`.
#'   \item uncertainty: Result from `compute_uncertainty()`.
#'   \item boundary: Result from `detect_boundary()`, or `NULL`.
#'   \item aggregation: Result from `detect_aggregation()`, or `NULL`.
#'   \item plots: A list of default plots if `plot = TRUE`, otherwise `NULL`.
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
    iter.max = 100,
    verbose = TRUE,
    reltol = 1e-4,
    initialnum = 10,
    return_all_fits = FALSE,
    uncertainty_normalized = TRUE,
    uncertainty_threshold_method = c("mean_sd", "quantile"),
    uncertainty_threshold_scale = 0.5,
    uncertainty_quantile_prob = NULL,
    run_boundary = TRUE,
    boundary_k = 10,
    run_aggregation = TRUE,
    aggregation_k = 10,
    aggregation_threshold_count = NULL,
    aggregation_threshold_prop = NULL,
    add_high_uncertain_step1 = TRUE,
    high_uncertainty_threshold_method = c("mean_sd", "quantile"),
    high_uncertainty_threshold_scale = 1.5,
    high_uncertainty_quantile_prob = NULL,
    aggregation_do_step2 = TRUE,
    aggregation_distance_threshold = 1,
    aggregation_min_component_size = 5,
    plot = FALSE
) {
  x <- as.matrix(x)
  dat_loc <- as.matrix(dat_loc)
  uncertainty_threshold_method <- match.arg(uncertainty_threshold_method)
  high_uncertainty_threshold_method <- match.arg(high_uncertainty_threshold_method)

  if (!is.numeric(x)) {
    stop("x must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (!is.numeric(dat_loc)) {
    stop("dat_loc must be a numeric matrix or coercible to a numeric matrix.")
  }
  if (nrow(x) != nrow(dat_loc)) {
    stop("x and dat_loc must have the same number of rows.")
  }
  if (!is.numeric(nclus) || length(nclus) != 1 || nclus < 2) {
    stop("nclus must be a single integer greater than or equal to 2.")
  }

  ## Step 1: clustering
  if (auto_m) {
    clustering <- fcmclust_auto(
      x = x,
      nclus = nclus,
      iter.max = iter.max,
      verbose = verbose,
      vec_m = vec_m,
      reltol = reltol,
      initialnum = initialnum,
      return_all_fits = return_all_fits
    )
  } else {
    clustering <- fcmclust(
      x = x,
      nclus = nclus,
      iter.max = iter.max,
      verbose = verbose,
      m = m,
      reltol = reltol,
      initialnum = initialnum
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
      k = boundary_k
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
      k = aggregation_k,
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
  if (plot) {
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
        is_boundary = boundary$loc_boundary,
        title = "Boundary",
        color_mode = "status"
      )
    }

    if (!is.null(aggregation)) {
      plots$aggregation <- plot_aggregation(
        dat_loc = dat_loc,
        cluster = clustering$cluster,
        is_uncertain = uncertainty$is_uncertain,
        is_aggregated = aggregation$is_aggregated,
        igraph_cluster = aggregation$igraph_cluster,
        title = "Aggregation",
        color_mode = "status"
      )
    }
  }

  settings <- list(
    auto_m = auto_m,
    uncertainty_normalized = uncertainty_normalized,
    uncertainty_threshold_method = uncertainty_threshold_method,
    uncertainty_threshold_scale = uncertainty_threshold_scale,
    uncertainty_quantile_prob = uncertainty_quantile_prob,
    run_boundary = run_boundary,
    boundary_k = boundary_k,
    run_aggregation = run_aggregation,
    aggregation_k = aggregation_k,
    aggregation_threshold_count = aggregation_threshold_count,
    aggregation_threshold_prop = aggregation_threshold_prop,
    add_high_uncertain_step1 = add_high_uncertain_step1,
    high_uncertainty_threshold_method = high_uncertainty_threshold_method,
    high_uncertainty_threshold_scale = high_uncertainty_threshold_scale,
    high_uncertainty_quantile_prob = high_uncertainty_quantile_prob,
    aggregation_do_step2 = aggregation_do_step2,
    aggregation_distance_threshold = aggregation_distance_threshold,
    aggregation_min_component_size = aggregation_min_component_size,
    plot = plot,
    return_all_fits = return_all_fits
  )

  list(
    clustering = clustering,
    uncertainty = uncertainty,
    high_uncertainty = high_uncertainty,
    boundary = boundary,
    aggregation = aggregation,
    plots = plots,
    settings = settings,
    call = match.call()
  )
}
