# R package for ElevenLabs API

This package provides a simple way to interact with the [ElevenLabs API](https://api.elevenlabs.io/docs), for the most realistic and versatile AI speech from text.

## Installation

```r
devtools::install_github("freestatman/elevenlabs")
```

## Usage

```r
library(elevenlabs)

# get elevenlabs voice name and id
get_voices()

# Get your API key from https://elevenlabs.com
Sys.setenv(ELEVENLABS_API_KEY = "your_api_key")

text_to_speech(
  "Hello, how are you today?",
  api_key = Sys.getenv("ELEVENLABS_API_KEY"),
  voice_name = "Elli",
  model_id = "eleven_multilingual_v2",
  stability = 0.4,
  similarity_boost = 0.6
)
```

## Example Shiny app

```r
shiny::runApp(
  system.file("shiny/elevenlabs_app", package = "elevenlabs")
)
```

## License
MIT

