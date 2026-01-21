library(testthat)

# Load the functions to be tested
# source("path/to/file/with/get_voice_id_function.R")

# Define the test cases
test_that("get_voice_id returns the expected output", {
  api_key <- Sys.getenv("ELEVENLABS_API_KEY")
  skip_on_cran()
  skip_if_not(nzchar(api_key))

  voices <- get_voices(api_key = api_key)
  expect_true(nrow(voices) > 0)

  # Test case 1: Test the function with a valid voice name
  output <- get_voice_id(voices$name[1], api_key = api_key)
  expect_type(output, "character")
  expect_equal(length(output), 1)
  expect_true(nzchar(output))

  # Test case 2: Test the function with an invalid voice name
  output <- get_voice_id("Invalid Voice Name", api_key = api_key)
  expected_output <- character()
  expect_identical(output, expected_output)

  # Test case 3: Test the function with a lowercase voice name
  output <- get_voice_id(tolower(voices$name[1]), api_key = api_key)
  expect_type(output, "character")
  expect_equal(length(output), 1)
  expect_true(nzchar(output))
})
