
burst_filename <- function(filename, n) {
  digits <- ceiling(log10(n+1))
  paste0(tools::file_path_sans_ext(filename), "_", sprintf(paste0("%0",digits,"d"), 1:n), ".", tools::file_ext(filename))
}


# Known limitations:
# 1. Plots that have been sized by egg::setpanel_size() loose their gtable information
# 2. patchwork::plot_annotation() titles are not counting towards the dimensions

get_layout_size <- function(gg, units = c("mm", "cm", "in")) {
  if (class(gg)[1] %in% c("patchwork", "gg", "ggplot")) gg <- list(gg)
  units <- match.arg(units)

  pages <-
    map(gg, function(x) {
      if (!is.ggplot(x)) stop("Please provide a ggplot or list of ggplots as input to 'gg'")
      gtab <- patchwork:::plot_table(x, 'auto')

      width <- NA
      height <- NA
      if (all(as.character(gtab$widths) != "1null"))
        width <- grid::convertWidth(sum(gtab$widths) + unit(1, "mm"), unitTo = units, valueOnly = TRUE)
      if (all(as.character(gtab$heights) != "1null"))
        height <- grid::convertHeight(sum(gtab$heights) + unit(1, "mm"), unitTo = units, valueOnly = TRUE)

      tibble(width = width, height = height)
    }) %>%
    bind_rows()

  overall_width<- NA
  overall_height <- NA
  if (all(!is.na(pages$width)))
    overall_width <- max(pages$width, na.rm = TRUE)
  if (all(!is.na(pages$height)))
    overall_height <- max(pages$height, na.rm = TRUE)

  list(
    units = units,
    pages = pages,
    max = c(width = overall_width, height = overall_height)
    )
}

#' Create multipage layout from plots
#'
#' @param gg ggplot or list of ggplots
#' @param ncol Number of columns in the layout. If more plots are provided than fit on one page, a list of patchworks is returned.
#' @param nrow Number of rows in the layout. If more plots are provided than fit on one page, a list of patchworks is returned.
#' @param width Widths of individual plots. For absolute dimensions use somehting like `unit(40, "mm")`.
#' @param height Heights of individual plots. For absolute dimensions use somehting like `unit(40, "mm")`.
#' @param ... Other arguments passed on to `patchwork::wrap_plots()`
#'
#' @export
layout_plots <- function(gg, ncol = NULL, nrow = NULL, width = NULL, height = NULL, ...) {
  if (class(gg)[1] %in% c("patchwork", "gg", "ggplot")) gg <- list(gg)

  if (is.numeric(ncol) & is.numeric(nrow)) {
    plots_per_page <- nrow * ncol
  } else {
    plots_per_page <- length(gg)
  }
  pages <-
    split(gg, ceiling(seq_along(gg)/plots_per_page)) %>%
    map(., ~patchwork::wrap_plots(.x, ncol = ncol, nrow = nrow, widths = width, heights = height, guides = "collect", ...))
  unname(pages)
}

#' Create multipage layout from facets
#'
#' @param gg A ggplot
#' @param facet_var Descr.
#' @param ncol Descr.
#' @param nrow Descr.
#' @param width Descr.
#' @param height Descr.
#' @param ... Descr.
#'
#' @export
layout_facets <- function(gg, facet_var, ncol = NULL, nrow = NULL, width = NULL, height = NULL, ...) {
  df <-
    gg$data %>%
    nest(data = -{{facet_var}}) %>%
    arrange({{facet_var}})
  plots <-
    map2(df$data, df %>% pull({{facet_var}}),
         function(data, facet_title) {
           gg %+% data + ggtitle(facet_title)
         })
  layout_plots(plots, ncol = ncol, nrow = nrow, width = width, height = height, ...)
}

#' Save multipage layout to file
#'
#' This function takes a ggplot (for single page) or list of ggplots (for multi page) and writes them to file.
#' In case the input has absolute dimensions (e.g. as a result of `egg::set_panel_size()` or
#' `patchwork::plot_layout()`), width and height of the output are adjusted to fit the content.
#'
#' @param gg ggplot or list of ggplots
#'
#' @param filename Filename for export
#' @param device Device to use. Can either be a device function (e.g. `png()`), or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param path Path to save plot to (combined with filename).
#' @param scale Multiplicative scaling factor.
#' @param width,height,units Plot size in `units` ("mm", "cm", or "in").
#'   If not supplied, uses the size of current graphics device.
#'   In case the input has absolute dimensions after applying
#'   `patchwork::plot_layout()`, width and height of the output are adjusted to fit the content.
#' @param dpi Plot resolution. Also accepts a string input: "retina" (320), "print" (300), or "screen" (72). Applies only to raster output types.
#' @param limitsize When TRUE (the default), ggsave will not save images larger than 50x50 inches, to prevent the common error of specifying dimensions in pixels.
#' @param ... Other arguments passed on to the graphics device function, as specified by device.
#' @param return_input If `TRUE` the input ggplot or plotlist is returned after saving.
#' This enables the use of `bro_ggsave_paged()` within `dplyr` pipes.
#' @export
save_layout <- function(gg = last_plot(), filename, device = NULL, path = NULL, scale = 1,
                           width = NA, height = NA, units = c("mm", "cm", "in"), dpi = 300, limitsize = TRUE,
                           return_input = FALSE, multiple_files = FALSE, ...) {
  if (class(gg)[1] %in% c("patchwork", "gg", "ggplot")) gg <- list(gg)
  units <- match.arg(units)

  dimensions <- get_layout_size(gg, units)$max

  width_defined_by <- case_when(is.na(width) && is.na(dimensions[["width"]]) ~ "was not defined - system default used",
                                !is.na(width) ~ "was provided as parameter 'width' to bro_ggsave_paged()",
                                TRUE ~ "was inferred from layout width")
  height_defined_by <- case_when(is.na(height) && is.na(dimensions[["height"]]) ~ "was not defined - system default used",
                                 !is.na(height) ~ "was provided as parameter 'height' to bro_ggsave_paged()",
                                 TRUE ~ "was inferred from layout height")

  if (is.na(width)) width <- dimensions[["width"]]
  if (is.na(height)) height <- dimensions[["height"]]

  message("Device width ", width_defined_by)
  message("Device height ", height_defined_by)
  message("------------------------------------------------------")
  if (!is.na(width) && !is.na(height)) message("Saving ", round(width), " x ", round(height), " mm image")

  if (multiple_files) {
    filenames <- burst_filename(filename, length(gg))
    if (toupper(tools::file_ext(filename)) == "PDF") {
      map2(gg, filenames,
           function(x, y) {
             ggplot2::ggsave(plot = x, filename = y, device = device, path = path, scale = scale,
                             width = width, height = height, units = units, dpi = dpi, limitsize = limitsize,
                             useDingbats = FALSE)
           })
    } else {
      map2(gg, filenames,
           function(x, y) {
             ggplot2::ggsave(plot = x, filename = y, device = device, path = path, scale = scale,
                             width = width, height = height, units = units, dpi = dpi, limitsize = limitsize)
           })
    }
    if(return_input) return(gg)

  } else {

    # this code is adapted from ggplot2::ggsave()
    dpi <- ggplot2:::parse_dpi(dpi)
    dev <- ggplot2:::plot_dev(device, filename, dpi = dpi)
    dim <- ggplot2:::plot_dim(c(width, height), scale = scale, units = units, limitsize = limitsize)
    if (!is.null(path)) {
      filename <- file.path(path, filename)
    }
    old_dev <- grDevices::dev.cur()
    if (toupper(tools::file_ext(filename)) == "PDF") {
      dev(filename = filename, width = dim[1], height = dim[2], useDingbats = FALSE, ...)
    } else {
      dev(filename = filename, width = dim[1], height = dim[2], ...)
    }
    on.exit(utils::capture.output({
      grDevices::dev.off()
      if (old_dev > 1) grDevices::dev.set(old_dev)
    }))
    map(gg, ~grid::grid.draw(.x))
    invisible()
    if (return_input) return(gg)
  }
}
