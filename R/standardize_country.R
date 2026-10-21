standardize_country <- function(x) {
  lookup <- c("United States" = "USA",
              "United Kingdom" = "UK",
              "Scotland" = "UK",
              "Northern Ireland" = "UK",
              "the Netherlands" = "Netherlands",
              "Czechia" = "Czech Republic",
              "East Timor" = "Timor-Leste",
              "Trinidad and Tobago" = "Trinidad",
              "Faroe Islands (Denmark)" = "Faroe Islands",
              "Guadeloupe, France" = "Guadeloupe",
              "Guadeloupe Island" = "Guadeloupe")
  ifelse(x %in% names(lookup), lookup[x], x)
}