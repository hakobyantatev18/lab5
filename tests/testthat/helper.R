# Skip tests that need the Nobel API when there's no internet, or on CRAN
skip_if_no_api <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline("api.nobelprize.org")
}