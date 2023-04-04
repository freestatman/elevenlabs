#' Get voices
#'
#' This function retrieves a list of voices from the URL "https://api.elevenlabs.io/v1/voices"
#' and returns a tibble containing the voice_id, name, and preview_url for each voice.
#'
#' @return A tibble with columns voice_id, name, and preview_url.
#' @import dplyr
#' @import jsonlite
#'
#' @examples
#' get_voices()
#'
#' @keywords internal
get_voices <- function() {
  # get the list of voices
  voices <- jsonlite::fromJSON("https://api.elevenlabs.io/v1/voices")$voices
  voices %>% dplyr::select(voice_id, name, preview_url)
  #
  #               voice_id   name
  # 1 21m00Tcm4TlvDq8ikWAM Rachel
  # 2 AZnzlk1XvdvUeBnXmlld   Domi
  # 3 EXAVITQu4vr4xnSDxMaL  Bella
  # 4 ErXwobaYiN019PkySvjV Antoni
  # 5 MF3mGyEYCl7XYWbV9V6O   Elli
  # 6 TxGEqnHWrfWFTfGW9XjX   Josh
  # 7 VR6AewLTigWG4xSOukaG Arnold
  # 8 pNInz6obpgDQGcFmaJgB   Adam
  # 9 yoZ06aMxZJJ28mfd3POQ    Sam
}


#' Get Voice ID
#'
#' This function retrieves the ID associated with a specific voice name.
#'
#' @param voice_name A character string indicating the name of the desired voice.
#' @return Numeric ID associated with the given voice name.
#' @importFrom stringr str_to_title
#' @importFrom dplyr filter pull
#' @export
#'
#' @examples
#' get_voice_id("Adam")
#' get_voice_id("adam")
get_voice_id <- function(voice_name) {
  get_voices() %>%
    dplyr::filter(name == stringr::str_to_title(voice_name)) %>%
    dplyr::pull(voice_id)
}


#' Convert Text to Speech using Eleven Labs API
#'
#' This function converts text to speech using the Eleven Labs API. The output file is a mp3 format.
#'
#' @param text The text to convert to speech
#' @param api_key The API key for Eleven Labs API. Default is to use the API key stored in the ELEVENLABS_API_KEY environment variable.
#' @param voice_name The name of the voice to use. Default is "Elli".
#' @param output_file The name of the output file. Default is "output.mp3"
#' @param stability The stability setting, a numeric value between 0 and 1.
#'        Specifies how predictable voice intonation should be. Default is 0
#' @param similarity_boost The similarity boost setting, a numeric value between 0 and 1.
#'        Specifies how much the voice should resemble the recordings used to train the voice model. Default is 0
#' @param ... Additional arguments to be passed on to other methods
#' @return The name of the output file
#' @export
#' @examples
#'
#' text_to_speech("Hello, how are you today?")
#'
text_to_speech <- function(text,
                           api_key = Sys.getenv("ELEVENLABS_API_KEY"),
                           voice_name = "Elli",
                           output_file = "output.mp3",
                           stability = 0,
                           similarity_boost = 0,
                           ...) {
  text <- stringr::str_replace_all(text, "\n", " ")
  voice_id <- get_voice_id(voice_name)
  base_url <- glue::glue("https://api.elevenlabs.io/v1/text-to-speech/{voice_id}")
  data <- sprintf('{\n  "text": "{%s}",\n  "voice_settings": {\n    "stability": %f,\n    "similarity_boost": %f\n  }\n}', text, stability, similarity_boost)

  # request
  request <- httr2::request(base_url) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(
      accept = "audio/mpeg",
      `xi-api-key` = api_key,
      `Content-Type` = "application/json"
    ) %>%
    httr2::req_body_raw(
      data
    ) %>%
    httr2::req_retry(max_tries = 3L) %>%
    httr2::req_error(is_error = function(...) FALSE)

  # perform the request
  request %>% req_dry_run()
  resp <- request %>%
    req_perform()

  # get the audio file
  httr2::resp_body_raw(resp) %>%
    writeBin(con = output_file)
  invisible(output_file)
}
