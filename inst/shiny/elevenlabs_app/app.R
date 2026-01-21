library(shiny)
library(elevenlabs)

audio_dir <- file.path(tempdir(), "elevenlabs_audio")
if (!dir.exists(audio_dir)) {
  dir.create(audio_dir, recursive = TRUE)
}
addResourcePath("elevenlabs_audio", audio_dir)

ui <- fluidPage(
  titlePanel("ElevenLabs Text-to-Speech"),
  sidebarLayout(
    sidebarPanel(
      passwordInput(
        "api_key",
        "API key",
        value = Sys.getenv("ELEVENLABS_API_KEY"),
        placeholder = "Paste your ElevenLabs API key"
      ),
      actionButton("refresh_voices", "Refresh voices"),
      selectInput("voice_name", "Voice", choices = "Elli"),
      textAreaInput(
        "text",
        "Text",
        value = "Hello, this is an ElevenLabs demo from R.",
        rows = 4
      ),
      textInput("model_id", "Model ID", value = "eleven_multilingual_v2"),
      sliderInput("stability", "Stability", min = 0, max = 1, value = 0.4, step = 0.05),
      sliderInput("similarity", "Similarity boost", min = 0, max = 1, value = 0.6, step = 0.05),
      sliderInput("style", "Style", min = 0, max = 1, value = 0, step = 0.05),
      checkboxInput("speaker_boost", "Use speaker boost", value = TRUE),
      selectInput(
        "output_format",
        "Output format",
        choices = c("mp3_44100_128", "mp3_44100_192"),
        selected = "mp3_44100_128"
      ),
      actionButton("synthesize", "Synthesize")
    ),
    mainPanel(
      uiOutput("audio_player"),
      verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  status_message <- reactiveVal("Ready.")
  audio_path <- reactiveVal(NULL)

  output$status <- renderText(status_message())

  observeEvent(input$refresh_voices, {
    if (!nzchar(input$api_key)) {
      status_message("Set an API key to load voices.")
      return(NULL)
    }

    voices <- tryCatch(
      get_voices(api_key = input$api_key),
      error = function(e) {
        status_message(e$message)
        NULL
      }
    )

    if (!is.null(voices) && nrow(voices) > 0) {
      updateSelectInput(
        session,
        "voice_name",
        choices = voices$name,
        selected = voices$name[1]
      )
      status_message(paste("Loaded", nrow(voices), "voices."))
    }
  })

  observeEvent(input$synthesize, {
    if (!nzchar(input$api_key)) {
      status_message("Set an API key before synthesizing audio.")
      return(NULL)
    }

    status_message("Generating audio...")
    output_file <- file.path(
      audio_dir,
      paste0("speech-", as.integer(Sys.time()), ".mp3")
    )

    tryCatch(
      text_to_speech(
        text = input$text,
        api_key = input$api_key,
        voice_name = input$voice_name,
        output_file = output_file,
        model_id = input$model_id,
        stability = input$stability,
        similarity_boost = input$similarity,
        style = input$style,
        use_speaker_boost = input$speaker_boost,
        output_format = input$output_format
      ),
      error = function(e) {
        status_message(e$message)
        NULL
      }
    )

    if (file.exists(output_file)) {
      audio_path(output_file)
      status_message(paste("Saved audio to", output_file))
    }
  })

  output$audio_player <- renderUI({
    path <- audio_path()
    if (is.null(path)) {
      return(NULL)
    }
    tags$audio(src = file.path("elevenlabs_audio", basename(path)), controls = NA)
  })
}

shinyApp(ui, server)
