#1.Big query
test_that("downloading all laureates returns the full dataset", {
  skip_if_no_api()
  x <- get_laureates()
  # Over 1000 prizes have been awarded; far more than one page of 25 so this only passes if pagination works
  expect_gt(nrow(x), 900)
})

#2.Limits of the API configuration

test_that("pagination works when results span several pages", {
  skip_if_no_api()
  # Page size 2 forces 2 requests for the 3 physics laureates of 2023
  raw <- nobel_request("laureates",
                       list(nobelPrizeCategory = "phy", nobelPrizeYear = 2023),
                       page_size = 2)
  expect_length(raw, 3)
})

test_that("a query with no results returns zero rows, not an error", {
  skip_if_no_api()
  # The Economics prize was first awarded in 1969
  expect_equal(nrow(get_laureates("economics", 1950)), 0)
})

test_that("years outside the range of the prize are rejected", {
  expect_error(get_laureates(year = 1900))
  expect_error(get_laureates(year = 3000))
})

#3.Inputs and outputs 

test_that("invalid inputs give errors", {
  expect_error(get_laureates(category = "cooking"))
  expect_error(get_laureates(category = 5))
  expect_error(get_laureates(category = c("physics", "peace")))
  expect_error(get_laureates(year = "2023"))
  expect_error(get_laureates(year = c(2000, 2001)))
})

test_that("output is a data frame with the documented columns and types", {
  skip_if_no_api()
  x <- get_laureates("physics", 2023)
  expect_s3_class(x, "data.frame")
  expect_named(x, c("id", "name", "gender", "birth_date", "birth_country",
                    "year", "category", "motivation", "prize_amount",
                    "birth_year", "age_at_award"))
  expect_type(x$year, "integer")
  expect_true(all(x$category == "Physics"))
  expect_true(all(x$year == 2023))
})

#4.Known values for specific inputs 

test_that("2023 physics returns the three known laureates", {
  skip_if_no_api()
  x <- get_laureates("physics", 2023)
  expect_equal(nrow(x), 3)
  expect_true(any(grepl("Agostini", x$name)))
  expect_true(any(grepl("Krausz", x$name)))
  expect_true(any(grepl("Huillier", x$name)))
})

test_that("Marie Curie only appears with the prize that was asked for", {
  skip_if_no_api()
  x <- get_laureates("chemistry", 1911)
  expect_equal(nrow(x), 1)
  expect_match(x$name, "Curie")
  expect_equal(x$category, "Chemistry")
})

test_that("organisations are handled (no gender, name from orgName)", {
  skip_if_no_api()
  x <- get_laureates("peace", 1917)
  expect_match(x$name, "Red Cross")
  expect_true(is.na(x$gender))
})
