library(testthat)
library(elevenlabs)

test_that("Helper functions work", {
  # Testing internal helpers
  expect_equal(
    elevenlabs:::.elevenlabs_compact_list(list(a = 1, b = NULL)),
    list(a = 1)
  )
  expect_equal(
    elevenlabs:::.elevenlabs_normalize_api_url("https://api.test/"),
    "https://api.test"
  )
  expect_equal(
    elevenlabs:::.elevenlabs_validate_api_key(" test_key "),
    "test_key"
  )
})

test_that("get_voices throws error on missing api key", {
  # Temporarily unset the env var to ensure error is thrown
  withr::with_envvar(list(ELEVENLABS_API_KEY = ""), {
    expect_error(get_voices(api_key = ""), "Please set ELEVENLABS_API_KEY")
  })
})

# Note: We cannot test success cases for get_voices without mocking,
# as we don't have a valid API key in this environment.
# Integration tests should be run in an environment with valid credentials.
