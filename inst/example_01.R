# set system variables for ElevenLabs API
library(here)
library(stringr)
library(httr2)
library(devtools) # make sure to have devtools 1.4!
library(glue)

Sys.setenv("ELEVENLABS_API_KEY" = "your_api_key")
Sys.setenv("ELEVENLABS_API_URL" = "https://api.elevenlabs.io/v1")
Sys.getenv("ELEVENLABS_API_KEY")

get_voices()$name
get_voices()$name %>%
  purrr::map(~ text_to_speech(text = glue::glue("Hello, my name is {.x}. I am a robot."), voice_name = .x, output_file = glue::glue("output-{.x}.mp3")))

text_to_speech(
  text = "Hello, my name is Adam. I am a robot.",
  voice_name = "Adam",
  output_file = "output.mp3",
  model_id = "eleven_multilingual_v2",
  stability = 0.8,
  similarity_boost = 0.5,
  style = 0.2,
  use_speaker_boost = TRUE
)

seq(0, 1, 0.2) %>%
  purrr::map(~ text_to_speech(
    text = glue::glue("Hello, my name is Adam. I am a robot."),
    voice_name = "Adam",
    stability = .x,
    output_file = glue::glue("output-s{.x}.mp3")
  ))

fs::dir_ls()


if (TRUE) { # example

  text <-
    " GPT-4 is the latest and most advanced version of OpenAI's large language model that can generate natural language and code from text and image inputs. It is a successor to GPT-3 and ChatGPT, which are already impressive AI systems that can interact in conversational dialogue form and perform various tasks. But what sets GPT-4 apart from its predecessors? Here are five ways GPT-4 outsmarts ChatGPT: \n
  1. GPT-4 has better general knowledge and problem-solving abilities. \n
  GPT-4 has been trained on a larger and more diverse dataset than ChatGPT, which includes more domains, languages, and modalities. This gives GPT-4 a broader general knowledge base and allows it to handle more complex tasks with greater accuracy. For example, GPT-4 can pass a simulated bar exam with a score around the top 10% of test takers; in contrast, ChatGPT's score was around the bottom 10%. GPT-4 can also solve problems that require reasoning based on images or charts, such as suggesting recipes from ingredients or explaining data trends.\n
  2. GPT-4 is more reliable and creative.\n
  GPT-4 has been aligned using human feedback to improve its factuality, steerability, and safety. This means that GPT-4 is less likely to produce incorrect or nonsensical answers than ChatGPT, which sometimes hallucinates information or exhibits problematic social biases. GPT-4 can also admit its mistakes, challenge incorrect premises, and reject inappropriate requests. Moreover, GPT-4 is more creative than ChatGPT in generating content that is relevant, coherent, and engaging. "

  stringr::str_wrap(text, width = 80)
  stringr::str_replace_all(text, "\n", " ")
  glue::trim(text)

  text_to_speech(
    text = text,
    voice_name = "Adam",
    output_file = "gpt4-outsmarts-chatgpt.mp3",
    stability = 0.8,
    similarity_boost = 0.5
  )
} # End example
