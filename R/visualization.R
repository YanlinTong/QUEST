#' Visualize spatial cluster labels
#'
#' @param dat_loc A numeric matrix or data frame of spatial coordinates
#'   with two columns.
#' @param cluster A vector of cluster labels of length n.
#' @param title Character; facet title.
#' @param palette Optional character vector of colors. If NULL, ggplot default
#'   discrete colors are used.
#' @param point_size Numeric; point size.
#' @param point_alpha Numeric; point alpha.
#' @param legend_nrow Integer; number of rows in legend.
#' @param legend_position Character; legend position.
#' @param strip_text_size Numeric; facet strip text size.
#' @param legend_text_size Numeric; legend text size.
#' @param legend_title_size Numeric; legend title size.
#' @param ... Additional arguments passed to `ggplot2::geom_point()`.
#'
#' @return A ggplot object.
#' @export
plot_clusters <- function(
    dat_loc,
    cluster,
    title = "Cluster",
    palette = NULL,
    point_size = 1.45,
    point_alpha = 1,
    legend_nrow = 2,
    legend_position = "bottom",
    strip_text_size = 16,
    legend_text_size = 12,
    legend_title_size = 12,
    ...
) {
  dat_loc <- as.data.frame(dat_loc)

  if (ncol(dat_loc) < 2) {
    stop("dat_loc must have at least two columns.")
  }
  if (length(cluster) != nrow(dat_loc)) {
    stop("cluster must have length equal to nrow(dat_loc).")
  }

  datt <- data.frame(
    x = dat_loc[[1]],
    y = dat_loc[[2]],
    cluster = factor(cluster),
    title = title
  )

  p <- ggplot2::ggplot(
    datt,
    ggplot2::aes(x = x, y = y, color = cluster)
  ) +
    ggplot2::geom_point(
      alpha = point_alpha,
      size = point_size,
      ...
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "", color = "Cluster") +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(size = 5),
        nrow = legend_nrow
      )
    ) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      strip.text.x = ggplot2::element_text(size = strip_text_size, face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(. ~ title)

  if (!is.null(palette)) {
    p <- p + ggplot2::scale_color_manual(values = palette)
  }

  p
}

#' Visualize uncertainty on spatial coordinates
#'
#' Uncertainty can be displayed either as a continuous variable or as
#' categorized levels based on mean and standard deviation thresholds.
#'
#' @param dat_loc A numeric matrix or data frame of spatial coordinates
#'   with two columns.
#' @param uncertainty A numeric vector of uncertainty values of length n.
#' @param mode Character; visualization mode. One of `"continuous"` or
#'   `"category"`.
#' @param title Character; facet title.
#' @param point_size Numeric; point size.
#' @param point_alpha Numeric; point alpha.
#' @param tile_alpha Numeric; tile alpha.
#' @param add_tile Logical; whether to add `geom_tile()`.
#' @param relabel Logical; whether to relabel category levels as
#'   "Certain", "Uncertain (moderate)", "Uncertain (high)", and
#'   "Uncertain (extreme)" when `mode = "category"`.
#' @param legend_position Character; legend position.
#' @param strip_text_size Numeric; facet strip text size.
#' @param legend_text_size Numeric; legend text size.
#' @param legend_title_size Numeric; legend title size.
#' @param legend_nrow Integer; number of rows in legend when
#'   `mode = "category"`.
#' @param ... Additional arguments passed to `ggplot2::geom_point()`.
#'
#' @return A list with:
#' \itemize{
#'   \item plot: A ggplot object.
#'   \item uncertainty_value: The input uncertainty vector.
#'   \item uncertainty_category: A factor of categorized uncertainty values
#'     when `mode = "category"`, otherwise `NULL`.
#' }
#' @export
plot_uncertainty <- function(
    dat_loc,
    uncertainty,
    mode = c("continuous", "category"),
    title = "Uncertainty",
    point_size = 1.45,
    point_alpha = 1,
    tile_alpha = 1,
    add_tile = TRUE,
    relabel = TRUE,
    legend_position = "bottom",
    strip_text_size = 16,
    legend_text_size = 12,
    legend_title_size = 12,
    legend_nrow = 4,
    ...
) {
  mode <- match.arg(mode)
  dat_loc <- as.data.frame(dat_loc)

  if (ncol(dat_loc) < 2) {
    stop("dat_loc must have at least two columns.")
  }
  if (length(uncertainty) != nrow(dat_loc)) {
    stop("uncertainty must have length equal to nrow(dat_loc).")
  }

  datt <- data.frame(
    x = dat_loc[[1]],
    y = dat_loc[[2]],
    uncertainty = uncertainty,
    title = title
  )

  uncertainty_category <- NULL

  if (mode == "continuous") {
    p <- ggplot2::ggplot(
      datt,
      ggplot2::aes(x = x, y = y, colour = uncertainty)
    )

    if (add_tile) {
      p <- p + ggplot2::geom_tile(alpha = tile_alpha)
    }

    p <- p +
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        ...
      ) +
      ggplot2::scale_color_viridis_c(
        option = "A",
        direction = -1,
        begin = 0,
        end = 1
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "", color = "Uncertainty") +
      ggplot2::theme(
        legend.position = legend_position,
        legend.text = ggplot2::element_text(size = legend_text_size),
        legend.title = ggplot2::element_text(size = legend_title_size),
        strip.text.x = ggplot2::element_text(size = strip_text_size, face = "bold"),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::facet_grid(. ~ title)
  }

  if (mode == "category") {
    mu <- mean(uncertainty, na.rm = TRUE)
    s <- stats::sd(uncertainty, na.rm = TRUE)

    uncertainty_category <- ifelse(
      uncertainty >= (mu + 1.5 * s), ">mean+1.5sd",
      ifelse(
        uncertainty >= (mu + 1.0 * s), "mean+sd ~ mean+1.5sd",
        ifelse(
          uncertainty >= (mu + 0.5 * s), "mean+0.5sd ~ mean+sd",
          "<mean+0.5sd"
        )
      )
    )

    uncertainty_category <- factor(
      uncertainty_category,
      levels = c(
        "<mean+0.5sd",
        "mean+0.5sd ~ mean+sd",
        "mean+sd ~ mean+1.5sd",
        ">mean+1.5sd"
      )
    )

    if (relabel) {
      uncertainty_category <- factor(
        uncertainty_category,
        levels = c(
          "<mean+0.5sd",
          "mean+0.5sd ~ mean+sd",
          "mean+sd ~ mean+1.5sd",
          ">mean+1.5sd"
        ),
        labels = c(
          "Certain",
          "Uncertain (moderate)",
          "Uncertain (high)",
          "Uncertain (extreme)"
        )
      )
    }

    datt$uncertainty_category <- uncertainty_category

    p <- ggplot2::ggplot(
      datt,
      ggplot2::aes(x = x, y = y, colour = uncertainty_category)
    )

    if (add_tile) {
      p <- p + ggplot2::geom_tile(alpha = tile_alpha)
    }

    p <- p +
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        ...
      ) +
      ggplot2::scale_colour_brewer(type = "seq", palette = "YlOrBr") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "", color = "Uncertainty") +
      ggplot2::guides(
        colour = ggplot2::guide_legend(
          override.aes = list(size = 5, fill = "white", linetype = rep(0, 4)),
          nrow = legend_nrow
        )
      ) +
      ggplot2::theme(
        legend.position = legend_position,
        legend.text = ggplot2::element_text(size = legend_text_size),
        legend.title = ggplot2::element_text(size = legend_title_size),
        strip.text.x = ggplot2::element_text(size = strip_text_size, face = "bold"),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::facet_grid(. ~ title)
  }

  list(
    plot = p,
    uncertainty_value = uncertainty,
    uncertainty_category = uncertainty_category
  )
}




#' Visualize boundary uncertain locations
#'
#' @param dat_loc A numeric matrix or data frame of spatial coordinates
#'   with two columns.
#' @param cluster A vector of cluster labels of length n.
#' @param is_uncertain A logical vector of length n.
#' @param is_boundary A logical vector of length n, or a vector of indices of
#'   boundary locations.
#' @param title Character; facet title.
#' @param color_mode Character; either `"status"` or `"cluster"`.
#'   If `"status"`, points are colored as certain / uncertain / boundary.
#'   If `"cluster"`, points are colored by cluster label and boundary locations
#'   are highlighted with a larger outline.
#' @param palette Optional character vector of colors for cluster labels when
#'   `color_mode = "cluster"`.
#' @param point_size Numeric; point size for background points.
#' @param boundary_size Numeric; point size for boundary points.
#' @param point_alpha Numeric; point alpha.
#' @param legend_position Character; legend position.
#' @param strip_text_size Numeric; facet strip text size.
#' @param legend_text_size Numeric; legend text size.
#' @param legend_title_size Numeric; legend title size.
#' @param ... Additional arguments passed to `ggplot2::geom_point()`.
#'
#' @return A ggplot object.
#' @export
plot_boundary <- function(
    dat_loc,
    cluster,
    is_uncertain,
    is_boundary,
    title = "Boundary",
    color_mode = c("status", "cluster"),
    palette = NULL,
    point_size = 1.45,
    boundary_size = 2.2,
    point_alpha = 1,
    legend_position = "bottom",
    strip_text_size = 16,
    legend_text_size = 12,
    legend_title_size = 12,
    ...
) {
  color_mode <- match.arg(color_mode)
  dat_loc <- as.data.frame(dat_loc)
  cluster <- as.vector(cluster)
  is_uncertain <- as.logical(is_uncertain)

  if (ncol(dat_loc) < 2) {
    stop("dat_loc must have at least two columns.")
  }

  n <- nrow(dat_loc)

  if (length(cluster) != n) {
    stop("cluster must have length equal to nrow(dat_loc).")
  }
  if (length(is_uncertain) != n) {
    stop("is_uncertain must have length equal to nrow(dat_loc).")
  }

  is_boundary_full <- rep(FALSE, n)
  if (is.logical(is_boundary)) {
    if (length(is_boundary) != n) {
      stop("Logical is_boundary must have length equal to nrow(dat_loc).")
    }
    is_boundary_full <- is_boundary
  } else {
    is_boundary_full[as.integer(is_boundary)] <- TRUE
  }

  datt <- data.frame(
    x = dat_loc[[1]],
    y = dat_loc[[2]],
    cluster = factor(cluster),
    is_uncertain = is_uncertain,
    is_boundary = is_boundary_full,
    title = title
  )

  datt$status <- "Certain"
  datt$status[datt$is_uncertain] <- "Uncertain"
  datt$status[datt$is_boundary] <- "Boundary"
  datt$status <- factor(
    datt$status,
    levels = c("Certain", "Uncertain", "Boundary")
  )

  if (color_mode == "status") {
    p <- ggplot2::ggplot(
      datt,
      ggplot2::aes(x = x, y = y, colour = status)
    ) +
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        ...
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "Certain" = "grey70",
          "Uncertain" = "steelblue",
          "Boundary" = "red"
        )
      )
  } else {
    p <- ggplot2::ggplot(
      datt,
      ggplot2::aes(x = x, y = y, colour = cluster)
    ) +
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        ...
      ) +
      ggplot2::geom_point(
        data = datt[datt$is_boundary, , drop = FALSE],
        shape = 1,
        size = boundary_size,
        stroke = 1.1,
        colour = "black"
      )

    if (!is.null(palette)) {
      p <- p + ggplot2::scale_color_manual(values = palette)
    }
  }

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "", color = "") +
    ggplot2::theme(
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      strip.text.x = ggplot2::element_text(size = strip_text_size, face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(. ~ title)

  p
}




#' Visualize aggregated uncertain locations
#'
#' @param dat_loc A numeric matrix or data frame of spatial coordinates
#'   with two columns.
#' @param cluster A vector of cluster labels of length n.
#' @param is_uncertain A logical vector of length n.
#' @param is_aggregated A logical vector of length n, or a vector of indices of
#'   aggregated locations.
#' @param igraph_cluster Optional vector of ordered igraph cluster labels of
#'   length n. Non-aggregated locations can be `NA`.
#' @param title Character; facet title.
#' @param color_mode Character; one of `"status"`, `"cluster"`, or `"igraph"`.
#' @param palette Optional character vector of colors for cluster or igraph labels.
#' @param point_size Numeric; point size for background points.
#' @param aggre_size Numeric; point size for aggregated points when highlighted.
#' @param point_alpha Numeric; point alpha.
#' @param legend_position Character; legend position.
#' @param strip_text_size Numeric; facet strip text size.
#' @param legend_text_size Numeric; legend text size.
#' @param legend_title_size Numeric; legend title size.
#' @param ... Additional arguments passed to `ggplot2::geom_point()`.
#'
#' @return A ggplot object.
#' @export
plot_aggregation <- function(
    dat_loc,
    cluster,
    is_uncertain,
    is_aggregated,
    igraph_cluster = NULL,
    title = "Aggregation",
    color_mode = c("status", "cluster", "igraph"),
    palette = NULL,
    point_size = 1.45,
    aggre_size = 2.2,
    point_alpha = 1,
    legend_position = "bottom",
    strip_text_size = 16,
    legend_text_size = 12,
    legend_title_size = 12,
    ...
) {
  color_mode <- match.arg(color_mode)
  dat_loc <- as.data.frame(dat_loc)
  cluster <- as.vector(cluster)
  is_uncertain <- as.logical(is_uncertain)

  if (ncol(dat_loc) < 2) {
    stop("dat_loc must have at least two columns.")
  }

  n <- nrow(dat_loc)

  if (length(cluster) != n) {
    stop("cluster must have length equal to nrow(dat_loc).")
  }
  if (length(is_uncertain) != n) {
    stop("is_uncertain must have length equal to nrow(dat_loc).")
  }

  is_aggregated_full <- rep(FALSE, n)
  if (is.logical(is_aggregated)) {
    if (length(is_aggregated) != n) {
      stop("Logical is_aggregated must have length equal to nrow(dat_loc).")
    }
    is_aggregated_full <- is_aggregated
  } else {
    is_aggregated_full[as.integer(is_aggregated)] <- TRUE
  }

  if (color_mode == "igraph") {
    if (is.null(igraph_cluster)) {
      stop("Please provide igraph_cluster when color_mode = 'igraph'.")
    }
    if (length(igraph_cluster) != n) {
      stop("igraph_cluster must have length equal to nrow(dat_loc).")
    }
  }

  datt <- data.frame(
    x = dat_loc[[1]],
    y = dat_loc[[2]],
    cluster = factor(cluster),
    is_uncertain = is_uncertain,
    is_aggregated = is_aggregated_full,
    title = title
  )

  datt$status <- "Certain"
  datt$status[datt$is_uncertain] <- "Uncertain"
  datt$status[datt$is_aggregated] <- "Aggregation"
  datt$status <- factor(
    datt$status,
    levels = c("Certain", "Uncertain", "Aggregation")
  )

  if (!is.null(igraph_cluster)) {
    datt$igraph_cluster <- factor(igraph_cluster)
  }

  if (color_mode == "status") {
    p <- ggplot2::ggplot(
      datt,
      ggplot2::aes(x = x, y = y, colour = status)
    ) +
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        ...
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "Certain" = "grey70",
          "Uncertain" = "steelblue",
          "Aggregation" = "red"
        )
      )
  } else if (color_mode == "cluster") {
    p <- ggplot2::ggplot(
      datt,
      ggplot2::aes(x = x, y = y, colour = cluster)
    ) +
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        ...
      ) +
      ggplot2::geom_point(
        data = datt[datt$is_aggregated, , drop = FALSE],
        shape = 1,
        size = aggre_size,
        stroke = 1.1,
        colour = "black"
      )

    if (!is.null(palette)) {
      p <- p + ggplot2::scale_color_manual(values = palette)
    }
  } else {
    p <- ggplot2::ggplot(
      datt[datt$is_aggregated, , drop = FALSE],
      ggplot2::aes(x = x, y = y, colour = igraph_cluster)
    ) +
      ggplot2::geom_point(
        size = point_size,
        alpha = point_alpha,
        ...
      )

    if (!is.null(palette)) {
      p <- p + ggplot2::scale_color_manual(values = palette)
    }
  }

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "", color = "") +
    ggplot2::theme(
      legend.position = legend_position,
      legend.text = ggplot2::element_text(size = legend_text_size),
      legend.title = ggplot2::element_text(size = legend_title_size),
      strip.text.x = ggplot2::element_text(size = strip_text_size, face = "bold"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::facet_grid(. ~ title)

  p
}
