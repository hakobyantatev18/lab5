# flatten the nested JSON into one row per laureate-prize
parse_laureates <- function(raw) {
  if (length(raw) == 0) return(data.frame())
  
  rows <- lapply(raw, function(l) {
    name <- get_field(l, "knownName", "en")
    if (is.na(name)) name <- get_field(l, "orgName", "en")  # organisations
    
    prize_rows <- lapply(l$nobelPrizes, function(p) {
      data.frame(
        id            = get_field(l, "id"),
        name          = name,
        gender        = get_field(l, "gender"),
        birth_date    = get_field(l, "birth", "date"),
        birth_country = get_field(l, "birth", "place", "countryNow", "en"),
        year          = as.integer(get_field(p, "awardYear")),
        category      = get_field(p, "category", "en"),
        motivation    = get_field(p, "motivation", "en"),
        prize_amount  = get_field(p, "prizeAmount"),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, prize_rows)
  })
  
  df <- do.call(rbind, rows) #whole list of tables at once 
  # Take the year as text 
  df$birth_year <- as.integer(substr(df$birth_date, 1, 4)) #sometimes only year known
  df$age_at_award <- df$year - df$birth_year
  df
}

#' Get Nobel Prize laureates
#'
#' Downloads laureates from the Nobel Prize API, optionally filtered by
#' category and/or year.
#'
#' @param category A single category tex "physics", `NULL` as default means all categories.
#' @param year The award year (1901 and onwards). `NULL` as default means all years.
#'
#' @return A data frame with one row per laureate and prize, with columns
#'   `id`, `name`, `gender`, `birth_date`, `birth_country`, `year`, `category`,
#'   `motivation`, `prize_amount`, `birth_year`, `age_at_award`.
#'
#' @export
get_laureates <- function(category = NULL, year = NULL) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (!is.null(year) && (!is.numeric(year) || length(year) != 1 || year < 1901 || year > current_year)) {
    stop("`year` must be a single number between 1901 and ", current_year, ".",
         call. = FALSE)
  }
  
  code <- if (is.null(category)) NULL else category_code(category)
  
  raw <- nobel_request("laureates",
                       list(nobelPrizeCategory = code, nobelPrizeYear = year))
  df <- parse_laureates(raw)
  if (nrow(df) == 0) return(df)
  
  # Keep only the prizes that match the request
  if (!is.null(code)) {
    cats <- nobel_categories()
    df <- df[df$category == cats$label[cats$code == code], ]
  }
  if (!is.null(year)) df <- df[df$year == year, ]
  
  rownames(df) <- NULL
  df
}