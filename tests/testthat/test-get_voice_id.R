library(testthat)

# Load the functions to be tested
# source("path/to/file/with/get_voice_id_function.R")

# Define the test cases
test_that("get_voice_id returns the expected output", {
  # Test case 1: Test the function with a valid voice name
  output <- get_voice_id("Adam")
  expected_output <- "pNInz6obpgDQGcFmaJgB"
  expect_equal(output, expected_output)

  # Test case 2: Test the function with an invalid voice name
  output <- get_voice_id("Invalid Voice Name")
  expected_output <- character()
  expect_identical(output, expected_output)

  # Test case 3: Test the function with a lowercase voice name
  output <- get_voice_id("adam")
  expected_output <- "pNInz6obpgDQGcFmaJgB"
  expect_equal(output, expected_output)
})
