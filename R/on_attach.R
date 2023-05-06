.onAttach <- function(...) {
  if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
    packageStartupMessage("Default 'ggplot2' theme has been changed.")
    ggplot2::theme_set(
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.line = ggplot2::element_line(colour = "grey20", size = ggplot2::rel(1)),
        axis.ticks = ggplot2::element_line(colour = "grey20"),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot"
      )
    )
  }
  packageStartupMessage("Default 'ggplot2' colour and fill scales set to 'viridis'.")
  options(
    ggplot2.discrete.colour = function(...) ggplot2::scale_colour_viridis_d(..., begin = 0.15, end = 0.85),
    ggplot2.discrete.fill = function(...) ggplot2::scale_fill_viridis_d(..., begin = 0.15, end = 0.85),
    ggplot2.continuous.colour = function(...) ggplot2::scale_colour_viridis_c(..., begin = 0.15, end = 0.85),
    ggplot2.continuous.fill = function(...) ggplot2::scale_fill_viridis_c(..., begin = 0.15, end = 0.85)
  )
}
