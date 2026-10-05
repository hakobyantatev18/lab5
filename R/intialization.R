nobel_categories <- function() {
  data.frame(
    name  = c("physics", "chemistry", "medicine", "literature", "peace", "economics"),
    code  = c("phy", "che", "med", "lit", "pea", "eco"),
    label = c("Physics", "Chemistry", "Physiology or Medicine",
              "Literature", "Peace", "Economic Sciences")
  )
}

# Turn "Physics" / "physics" / "phy" into the API code "phy"
# Turn "Physics" / "physics" / "phy" into the API code "phy"
category_code <- function(category) {
  cats <- nobel_categories()
  
  key <- tolower(trimws(category))
  
  # Accept internal name, e.g. "physics", "medicine"
  if (key %in% cats$name) {
    return(cats$code[match(key, cats$name)])
  }
  
  # Accept API code, e.g. "phy", "med"
  if (key %in% cats$code) {
    return(key)
  }
  
  # Accept official label, e.g. "Physics",
  # "Physiology or Medicine", "Economic Sciences"
  label_key <- tolower(cats$label)
  
  if (key %in% label_key) {
    return(cats$code[match(key, label_key)])
  }
  
  stop(
    "Unknown category '", category,
    "'. Choose one of: ",
    paste(cats$name, collapse = ", "),
    call. = FALSE
  )
}
# Returns NA instead of crashing when a field is missing
get_field <- function(x, ...) {
  for (name in c(...)) {
    x <- x[[name]]
    if (is.null(x)) return(NA)
  }
  x
}

