liu_turkos <- c("#D1F4F6",  # 20%
                "#A2E9ED",  # 40%
                "#74DDE4",  # 60%
                "#45D2DB",  # 80%
                "#17C7D2")  # 100%


#' Map the birth countries of laureates
#'
#' @param laureates A data frame returned by [get_laureates()].
#' @return A ggplot object: a world map coloured by number of laureates born
#'   in each country.

#' @importFrom maps map
#' @export
plot_birth_countries <- function(laureates) {
  
  # Count laureates per country (table() drops organisations)
  counts <- as.data.frame(table(region = laureates$birth_country),
                          stringsAsFactors = FALSE)
  names(counts)[2] <- "laureates"
  
  world <- ggplot2::map_data("world") #world map outline 
  world <- merge(world, counts, by = "region", all.x = TRUE)
  world <- world[order(world$group, world$order), ] 
  
  ggplot2::ggplot(world, ggplot2::aes(x = .data$long, y = .data$lat,
                                      group = .data$group, fill = .data$laureates)) +
    ggplot2::geom_polygon(colour = "white", linewidth = 0.1) +
    ggplot2::scale_fill_gradientn(colours = liu_turkos,
                                  trans = "log10",
                                  na.value = "grey85",
                                  name = "Laureates") +
    ggplot2::coord_quickmap() +
    ggplot2::theme_light() +
    ggplot2::labs(title = "Birth countries of Nobel laureates")
}