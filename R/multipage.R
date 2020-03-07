
burst_filename <- function(filename, n) {
  digits <- ceiling(log10(n+1))
  paste0(tools::file_path_sans_ext(filename), "_", sprintf(paste0("%0",digits,"d"), 1:n), ".", tools::file_ext(filename))
}


# Known limitations:
# 1. Plots that have been sized by egg::setpanel_size() loose their gtable information
# 2. patchwork::plot_annotation() titles are not counting towards the dimensions

get_layout_size <- function(gg, units = c("mm", "cm", "in")) {
  if (is.ggplot(gg)) gg <- list(gg)
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

#' multipage_plots
#' @param gg A `ggplot` or list of `ggplot`s
#'
#' @param ncol,nrow The number of columns and rows per page.
#' @inheritParams patchwork::wrap_plots
#'
#' @export
multipage_plots <- function(gg,
                            ncol = NULL,
                            nrow = NULL,
                            byrow = NULL,
                            widths = NULL,
                            heights = NULL,
                            guides = "collect",
                            tag_level = NULL,
                            design = NULL) {
  if (!is.ggplot(gg) && !all(purrr::map_lgl(gg, is.ggplot)))
    stop("argument 'gg' should be ggplot or list off ggplots")
  if (is.ggplot(gg)) gg <- list(gg)

  if (is.numeric(ncol) & is.numeric(nrow)) {
    plots_per_page <- nrow * ncol
  } else {
    plots_per_page <- length(gg)
  }
  pages <-
    split(gg, ceiling(seq_along(gg)/plots_per_page)) %>%
    map(., ~patchwork::wrap_plots(.x, ncol = ncol, nrow = nrow, widths = widths, heights = heights, guides = guides, byrow = byrow, tag_level = tag_level, design = design))
  unname(pages)
}

#' multipage_facets
#' @param gg A `ggplot`
#'
#' @param facet_by Variable that should be used for faceting.
#' @param ncol,nrow The number of columns and rows per page.
#' @inheritParams patchwork::wrap_plots
#'
#' @export
multipage_facets <- function(gg,
                             facet_by,
                             ncol = NULL,
                             nrow = NULL,
                             byrow = NULL,
                             widths = NULL,
                             heights = NULL,
                             guides = "collect",
                             tag_level = NULL,
                             design = NULL) {
  if (!is.ggplot(gg))
    stop("argument 'gg' should be a single ggplot")
  if(missing(facet_by))
    stop("argument 'facet_by' missing without default")

  df <-
    gg$data %>%
    nest(data = -{{facet_by}}) %>%
    arrange({{facet_by}})
  plots <-
    map2(df$data, df %>% pull({{facet_by}}),
         function(data, facet_title) {
           gg %+% data + ggtitle(facet_title)
         })
  multipage_plots(plots, ncol = ncol, nrow = nrow, widths = widths, heights = heights, guides = guides, byrow = byrow, tag_level = tag_level, design = design)
}

#' Save multipage layout to file
#'
#' This function takes a `ggplot` or list of `ggplot`s and writes them to a (multipage) file. See below for details.
#' 
#' __Handling of multiple pages.__
#' For a list of `ggplot`s, each list element is rendered as a separate page into a mutipage `pdf` file.
#' To save pages as individual files, use `multiple_files = TRUE`.
#' For output formats that do not support multipage files (`png`, `jpg`, etc), pages are saved to individual files by default.
#'
#' __Handling of file dimensions.__
#' Output file dimensions are determined according the the following precedence.
#' 1) The `width` and `height` parameters of `save_multipage()`.
#' 2) Dimensions inferred from an incoming `ggplot` object containing absolute dimensions.
#' 3) System default device dimensions.
#'
#' @param gg A `ggplot` or list of `ggplot`s
#'
#' @param width,height Dimensions of the saved plot. If not specified, `save_multipage()` will
#' try to infer the dimensions from the incoming `ggplot` object. If the incoming `ggplot` object has no absolute
#' dimensions, system default device dimensions are used.
#' @param units Unit dimensions. Defaults to "mm".
#' @param return_input Return the input ggplot or plotlist is after saving.
#' This enables the use within `dplyr` pipes.
#' @param multiple_files Save pages as individal files.
#' @inheritParams ggplot2::ggsave
#'
#' @export
save_multipage <- function(gg = last_plot(), filename, device = NULL, path = NULL, scale = 1,
                           width = NA, height = NA, units = c("mm", "cm", "in"), dpi = 300, limitsize = TRUE,
                           return_input = FALSE, multiple_files = FALSE, ...) {
  if (!is.ggplot(gg) && !all(purrr::map_lgl(gg, is.ggplot)))
    stop("argument 'gg' should be ggplot or list off ggplots")

  if (is.ggplot(gg)) gg <- list(gg)
  units <- match.arg(units)
  
  if (length(gg) > 1 && toupper(tools::file_ext(filename)) != "PDF")
    multiple_files <- TRUE

  dimensions <- get_layout_size(gg, units)$max

  width_defined_by <- case_when(is.na(width) && is.na(dimensions[["width"]]) ~ "was not defined - system default used",
                                !is.na(width) ~ "was provided as parameter 'width' to save_layout()",
                                TRUE ~ "was inferred from layout width")
  height_defined_by <- case_when(is.na(height) && is.na(dimensions[["height"]]) ~ "was not defined - system default used",
                                 !is.na(height) ~ "was provided as parameter 'height' to save_layout()",
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
                             useDingbats = FALSE, ...)
           })
    } else {
      map2(gg, filenames,
           function(x, y) {
             ggplot2::ggsave(plot = x, filename = y, device = device, path = path, scale = scale,
                             width = width, height = height, units = units, dpi = dpi, limitsize = limitsize, ...)
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
