nobel_categories <- function() {
  data.frame(
    name  = c("physics", "chemistry", "medicine", "literature", "peace", "economics"),
    code  = c("phy", "che", "med", "lit", "pea", "eco"),
    label = c("Physics", "Chemistry", "Physiology or Medicine",
              "Literature", "Peace", "Economic Sciences")
  )
}

# Turn "Physics" / "physics" / "phy" into the API code "phy"
category_code <- function(category) {
  if (!is.character(category) || length(category) != 1) {
    stop("`category` must be a single character string.", call. = FALSE)
  }
  cats <- nobel_categories()
  key <- tolower(category)
  if (key %in% cats$code) return(key)
  if (!key %in% cats$name) {
    stop("Unknown category '", category, "'. Choose one of: ",
         paste(cats$name, collapse = ", "), call. = FALSE)
  }
  cats$code[cats$name == key]
}

# Returns NA instead of crashing when a field is missing
get_field <- function(x, ...) {
  for (name in c(...)) {
    x <- x[[name]]
    if (is.null(x)) return(NA)
  }
  x
}