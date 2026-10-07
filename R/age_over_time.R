#' Plot the age of laureates at the time of their award
#'
#' @param laureates A data frame returned by [get_laureates()].
#' @return A ggplot object: one point per laureate with a trend line, 
#' 
#' @importFrom ggplot2 .data
#' @importFrom ggplot2 ggplot geom_point geom_smooth
#' @export
plot_age_over_time <- function(laureates) {
  
  # liu colors 
  liu_turkos <- "#17C7D2"
  liu_dark   <- "#0E7A80"
  liu_light  <- "#D1F4F6"
  
  # Organisations have no birth date, so no age
  df <- laureates[!is.na(laureates$age_at_award), ]
  if (nrow(df) == 0) stop("No laureates with a known birth date.", call. = FALSE)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$age_at_award)) +
    ggplot2::geom_point(alpha = 0.6, colour = liu_turkos) +
    ggplot2::labs(x = "Award year", y = "Age at award",
                  title = "Age of Nobel laureates at the time of the award") +
    ggplot2::theme_light() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = liu_light, colour = NA),
      strip.text       = ggplot2::element_text(colour = liu_dark, face = "bold")
    )
  
  # A trend line but only if there are at least 10 points 
  if (nrow(df) >= 10) {
    p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                  colour = liu_dark, fill = liu_turkos, alpha = 0.2)
  }
  if (length(unique(df$category)) > 1) { #splits the plot into 1 panel per cat if more then 2 cats 
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$category))
  }
  p
}