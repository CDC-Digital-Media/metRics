# install.packages("testthat")
library(testthat)
source("UrlTools.R")

test_removes_default_html <- function() {
  url <- "https://www.cdc.gov/hiv/default.html"
  expect_equal(CleanUrl(url), "https://www.cdc.gov/hiv")
}

test_removes_index_html <- function() {
  url <- "https://www.cdc.gov/hiv/basics/index.html"
  expect_equal(CleanUrl(url), "https://www.cdc.gov/hiv/basics")
}

test_leaves_meaningful_page <- function() {
  url <- "http://www.cdc.gov/hiv/basics/prevention.html"
  expect_equal(CleanUrl(url), "https://www.cdc.gov/hiv/basics/prevention.html")
}

test_removes_trailing_slash <- function() {
  url <- "http://www.cdc.gov/hiv/"
  expect_equal(CleanUrl(url), "https://www.cdc.gov/hiv")
}

test_converts_to_https <- function() {
  url <- "http://www.cdc.gov/hiv"
  expect_equal(CleanUrl(url), "https://www.cdc.gov/hiv")
}

test_removes_default_html()
test_removes_index_html()
test_leaves_meaningful_page()
test_removes_trailing_slash()
test_converts_to_https()