#' Girlboss theme
#'
#' A very cute ggplot2 theme based off \href{https://twitter.com/sailorhg}{sailorhg}'s \href{https://sailorhg.github.io/fairyfloss/}{fairyfloss} text editor theme
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @rdname theme_girlboss
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(nintendo_sales, aes(x = sales_million, y = console)) +
#'   geom_col() +
#'   facet_wrap(~sales_type) +
#'   theme_girlboss()
#' @importFrom ggplot2 element_rect element_line element_text element_blank theme theme_grey
theme_girlboss <- function(base_size = 16, base_line_size = base_size / 22, base_rect_size = base_size / 22) {
  windowsFonts("Arial" = windowsFont("Arial New"))
  theme_grey(base_size = base_size, base_family = "Arial", base_line_size, base_rect_size) +
    theme(
      plot.background = element_rect(fill = girlboss_col("dark_purple"), colour = girlboss_col("light_purple")),
      panel.background = element_rect(fill = girlboss_col("dark_purple")),
      panel.grid.major = element_line(colour = girlboss_col("light_purple")),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(colour = girlboss_col("dark_turquoise")),
      axis.text = element_text(colour = girlboss_col("dark_turquoise")),
      axis.title = element_text(colour = girlboss_col("light_turquoise")),
      plot.title = element_text(colour = girlboss_col("dark_pink"), hjust = 0.5),
      plot.subtitle = element_text(colour = girlboss_col("dark_pink"), hjust = 0.5),
      legend.background = element_rect(fill = NA, colour = girlboss_col("light_purple")),
      legend.title = element_text(colour = girlboss_col("light_pink")),
      legend.text = element_text(colour = girlboss_col("seashell")),
      legend.key = element_rect(fill = NA),
      strip.background = element_rect(fill = girlboss_col("dark_pink")),
      strip.text = element_text(colour = girlboss_col("seashell"))
    )
}

#' Girlboss color scale
#'
#' @param discrete Whether the colour aesthetic is discrete or not
#' @param reverse Whether the palette should be reversed
#' @param ... Additional arguments
#'
#' @rdname scale_colour_girlboss
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(nintendo_sales, aes(x = sales_million, y = console, colour = sales_type)) +
#'   geom_point() +
#'   scale_color_girlboss() +
#'   theme_girlboss()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
scale_colour_girlboss <- function(discrete = TRUE, reverse = FALSE, ...) {
  pal <- girlboss_pal(reverse = reverse)

  if (discrete) {
    discrete_scale("colour", "girlboss", palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Girlboss fill scale
#'
#' @inheritParams scale_colour_girlboss
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(nintendo_sales, aes(x = sales_million, y = console, fill = sales_type)) +
#'   geom_col() +
#'   scale_fill_girlboss() +
#'   theme_girlboss()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
scale_fill_girlboss <- function(discrete = TRUE, reverse = FALSE, ...) {
  pal <- girlboss_pal(reverse = reverse)

  if (discrete) {
    discrete_scale("fill", "girlboss", palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' @importFrom grDevices colorRampPalette
girlboss_pal <- function(reverse = FALSE, ...) {
  pal <- girlboss_col()
  pal <- pal[!(names(pal) %in% c("dark_purple", "light_purple", "seashell"))]

  if (reverse) {
    pal <- rev(pal)
  }
  colorRampPalette(pal, ...)
}

#' @export
#' @rdname scale_colour_girlboss
scale_color_girlboss <- scale_colour_girlboss

#' Girlboss colours
#'
#' @param ...
#'
#' @examples
#' library(ggplot2)
#' n <- length(girlboss_col())
#' ggplot(data.frame(x = seq(1,n,1), y = rep(1,n), girlboss = girlboss_col()),
#'        aes(x,y,fill=girlboss)) +
#'   geom_tile() +
#'   scale_fill_manual(values = as.character(girlboss_col())) +
#'   theme_void() +
#'   coord_fixed(3)
girlboss_col <- function(...) {
  cols <- c(...)

  if (is.null(cols)) {
    return(girlboss_colours)
  }

  girlboss_colours[cols]
}

# girlboss_colours <-
#   c(
#     dark_purple = "#5A5475",
#     light_purple = "#B7A0CD",
#     dark_pink = "#E3837D",
#     light_pink = "#F5B6D0",
#     dark_yellow = "#E9C062",
#     light_yellow = "#FAF58A",
#     mint = "#C0FFDE",
#     blue = "#96CBFE",
#     white = "#FFFFFF"
#   )

girlboss_colours <-
  c(
    dark_purple = "#2E303C",
    light_purple = "#43516C",
    seashell = "#F2EDE7",
    dark_pink = "#DB9E9B",
    light_pink = "#F8D4D6",
    dark_turquoise = "#00AAB0",
    light_turquoise = "#DBF4F1"
    # mint = "#C0FFDE",
    # blue = "#96CBFE"
  )
