# R package for elevenlabs API

This package provides a simple way to interact with the elevenlabs API, for the most realistic and versatile AI speech from text.

## Installation

```r
devtools::install_github("freestatman/elevenlabs")
```

## Usage

```r
library(elevenlabs)

# get elevenlabs voice id
get_voice_id("Adam")

# Get your API key from https://elevenlabs.com
text_to_speech(
  "Hello, how are you today?",
  api_key = Sys.getenv("ELEVENLABS_API_KEY")
)
```

## License
MIT

