#' Get voices names
#'
#' This function retrieves a list of voices from the ElevenLabs API and returns
#' a tibble containing the voice_id, name, and preview_url for each voice.
#'
#' @param api_key The API key for ElevenLabs. Defaults to the
#'   ELEVENLABS_API_KEY environment variable.
#' @param api_url Base URL for the ElevenLabs API. Defaults to the
#'   ELEVENLABS_API_URL environment variable or "https://api.elevenlabs.io/v1".
#' @return A tibble with columns voice_id, name, preview_url, and category
#'   when available.
#' @import dplyr
#' @import jsonlite
#'
#' @examples
#' get_voices()
#'
#' @export
get_voices <- function(
    api_key = Sys.getenv("ELEVENLABS_API_KEY"),
    api_url = Sys.getenv("ELEVENLABS_API_URL", "https://api.elevenlabs.io/v1")
) {
  api_key <- .elevenlabs_validate_api_key(api_key)
  api_url <- .elevenlabs_normalize_api_url(api_url)

  request <- httr2::request(glue::glue("{api_url}/voices")) %>%
    httr2::req_method("GET") %>%
    httr2::req_headers(`xi-api-key` = api_key) %>%
    httr2::req_retry(max_tries = 3L)

  resp <- request %>% httr2::req_perform()
  voices <- jsonlite::fromJSON(httr2::resp_body_string(resp))$voices

  voices %>%
    dplyr::select(dplyr::any_of(c("voice_id", "name", "preview_url", "category")))
}


#' Get Voice ID
#'
#' This function retrieves the ID associated with a specific voice name.
#'
#' @param voice_name A character string indicating the name of the desired voice.
#' @param api_key The API key for ElevenLabs. Defaults to the
#'   ELEVENLABS_API_KEY environment variable.
#' @param api_url Base URL for the ElevenLabs API. Defaults to the
#'   ELEVENLABS_API_URL environment variable or "https://api.elevenlabs.io/v1".
#' @return Character ID associated with the given voice name.
#' @importFrom stringr str_to_lower
#' @importFrom dplyr filter pull
#' @export
#'
#' @examples
#' get_voice_id("Adam")
#' get_voice_id("adam")
get_voice_id <- function(
    voice_name,
    api_key = Sys.getenv("ELEVENLABS_API_KEY"),
    api_url = Sys.getenv("ELEVENLABS_API_URL", "https://api.elevenlabs.io/v1")
) {
  voices <- get_voices(api_key = api_key, api_url = api_url)

  matches <- voices %>%
    dplyr::filter(stringr::str_to_lower(name) ==
      stringr::str_to_lower(voice_name)) %>%
    dplyr::pull(voice_id)

  if (length(matches) > 1) {
    matches <- matches[1]
  }

  matches
}


#' Convert Text to Speech using Eleven Labs API
#'
#' This function converts text to speech using the Eleven Labs API. The output
#' file is a mp3 format by default.
#'
#' @param text The text to convert to speech
#' @param api_key The API key for ElevenLabs. Defaults to the
#'   ELEVENLABS_API_KEY environment variable.
#' @param voice_name The name of the voice to use. Default is "Elli".
#' @param voice_id Optional voice ID. If supplied, this takes precedence over
#'   voice_name.
#' @param output_file The name of the output file. Default is "output.mp3"
#' @param model_id The model ID to use for synthesis. Defaults to
#'   "eleven_multilingual_v2".
#' @param stability The stability setting, a numeric value between 0 and 1.
#'        Specifies how predictable voice intonation should be. Default is 0
#' @param similarity_boost The similarity boost setting, a numeric value between 0 and 1.
#'        Specifies how much the voice should resemble the recordings used to train the voice model. Default is 0
#' @param style The style setting, a numeric value between 0 and 1. Default is 0.
#' @param use_speaker_boost Whether to enable speaker boost. Default is TRUE.
#' @param output_format Output format for the audio. Defaults to
#'   "mp3_44100_128".
#' @param optimize_streaming_latency Optional optimization level for
#'   streaming latency.
#' @param seed Optional seed for deterministic generation.
#' @param previous_text Optional previous context text.
#' @param next_text Optional next context text.
#' @param api_url Base URL for the ElevenLabs API. Defaults to the
#'   ELEVENLABS_API_URL environment variable or "https://api.elevenlabs.io/v1".
#' @param ... Additional arguments to be passed on to other methods
#' @return The name of the output file
#' @export
#' @import httr2
#' @examples
#'
#' text_to_speech("Hello, how are you today?")
#'
text_to_speech <- function(
    text,
    api_key = Sys.getenv("ELEVENLABS_API_KEY"),
    voice_name = "Elli",
    voice_id = NULL,
    output_file = "output.mp3",
    model_id = "eleven_multilingual_v2",
    stability = 0,
    similarity_boost = 0,
    style = 0,
    use_speaker_boost = TRUE,
    output_format = "mp3_44100_128",
    optimize_streaming_latency = NULL,
    seed = NULL,
    previous_text = NULL,
    next_text = NULL,
    api_url = Sys.getenv("ELEVENLABS_API_URL", "https://api.elevenlabs.io/v1"),
    ...
) {
  api_key <- .elevenlabs_validate_api_key(api_key)
  api_url <- .elevenlabs_normalize_api_url(api_url)

  if (!nzchar(text)) {
    stop("`text` must be a non-empty string.", call. = FALSE)
  }

  text <- stringr::str_replace_all(text, "\n", " ")

  if (is.null(voice_id) || !nzchar(voice_id)) {
    voice_id <- get_voice_id(
      voice_name = voice_name,
      api_key = api_key,
      api_url = api_url
    )
  }

  if (length(voice_id) == 0 || !nzchar(voice_id[1])) {
    stop("Could not resolve a voice_id for the supplied voice_name.", call. = FALSE)
  }

  voice_id <- voice_id[1]

  base_url <- glue::glue("{api_url}/text-to-speech/{voice_id}")
  voice_settings <- .elevenlabs_compact_list(list(
    stability = stability,
    similarity_boost = similarity_boost,
    style = style,
    use_speaker_boost = use_speaker_boost
  ))

  payload <- .elevenlabs_compact_list(list(
    text = text,
    model_id = model_id,
    voice_settings = voice_settings,
    seed = seed,
    previous_text = previous_text,
    next_text = next_text
  ))

  request <- httr2::request(base_url) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(
      accept = "audio/mpeg",
      `xi-api-key` = api_key,
      `Content-Type` = "application/json"
    ) %>%
    httr2::req_body_json(payload, auto_unbox = TRUE) %>%
    httr2::req_retry(max_tries = 3L)

  query <- .elevenlabs_compact_list(list(
    output_format = output_format,
    optimize_streaming_latency = optimize_streaming_latency
  ))

  if (length(query) > 0) {
    request <- do.call(httr2::req_url_query, c(list(request), query))
  }

  resp <- request %>% httr2::req_perform()

  httr2::resp_body_raw(resp) %>%
    writeBin(con = output_file)
  invisible(output_file)
}

.elevenlabs_normalize_api_url <- function(api_url) {
  if (is.null(api_url) || !nzchar(api_url)) {
    stop("`api_url` must be a non-empty string.", call. = FALSE)
  }
  sub("/+$", "", api_url)
}

.elevenlabs_validate_api_key <- function(api_key) {
  if (is.null(api_key) || !nzchar(api_key)) {
    stop(
      "Please set ELEVENLABS_API_KEY or pass `api_key` explicitly.",
      call. = FALSE
    )
  }
  api_key
}

.elevenlabs_compact_list <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}
