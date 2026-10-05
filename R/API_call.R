nobel_request <- function(endpoint, query = list(), page_size = 100) {
  results <- list()
  offset <- 0
  
  repeat {
    resp <- httr2::request("https://api.nobelprize.org/2.1/") |>
      httr2::req_url_path_append(endpoint) |>
      httr2::req_url_query(!!!query, limit = page_size, offset = offset) |>
      httr2::req_perform()
    
    body <- httr2::resp_body_json(resp)
    page <- body[[endpoint]]
    results <- c(results, page)
    
    offset <- offset + page_size
    if (length(page) == 0 || offset >= body$meta$count) break
  }
  
  results
}