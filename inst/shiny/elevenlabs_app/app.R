library(shiny)
library(bslib)
library(elevenlabs)
library(DT)

# Setup audio directory in temp (or a specific location if persistent, but temp for now)
audio_dir <- file.path(tempdir(), "elevenlabs_audio")
if (!dir.exists(audio_dir)) {
  dir.create(audio_dir, recursive = TRUE)
}
addResourcePath("elevenlabs_audio", audio_dir)

ui <- page_sidebar(
  title = "ElevenLabs Text-to-Speech",
  theme = bs_theme(preset = "flatly", version = 5),

  sidebar = sidebar(
    title = "Controls",
    width = 350,
    passwordInput(
      "api_key",
      "API Key",
      value = Sys.getenv("ELEVENLABS_API_KEY"),
      placeholder = "Paste your ElevenLabs API key"
    ),

    layout_columns(
      actionButton("refresh_voices", "Refresh Voices", icon = icon("sync")),
      col_widths = 12
    ),

    selectInput("voice_name", "Voice", choices = "Elli"),

    textAreaInput(
      "text",
      "Text",
      value = "Hello, this is an ElevenLabs demo from R.",
      rows = 4,
      resize = "vertical"
    ),

    accordion(
      open = FALSE,
      accordion_panel(
        "Advanced Settings",
        textInput("model_id", "Model ID", value = "eleven_multilingual_v2"),
        sliderInput(
          "stability",
          "Stability",
          min = 0,
          max = 1,
          value = 0.5,
          step = 0.05
        ),
        sliderInput(
          "similarity",
          "Similarity Boost",
          min = 0,
          max = 1,
          value = 0.75,
          step = 0.05
        ),
        sliderInput("style", "Style", min = 0, max = 1, value = 0, step = 0.05),
        checkboxInput("speaker_boost", "Use speaker boost", value = TRUE),
        selectInput(
          "output_format",
          "Output format",
          choices = c("mp3_44100_128", "mp3_44100_192"),
          selected = "mp3_44100_128"
        )
      )
    ),

    div(
      class = "d-grid gap-2 mt-3",
      actionButton(
        "synthesize",
        "Generate Audio",
        class = "btn-primary btn-lg",
        icon = icon("wand-magic-sparkles")
      )
    )
  ),

  # Main Content
  layout_columns(
    col_widths = c(12, 12),

    # Player Card
    card(
      card_header("Audio Player", class = "bg-primary text-white"),
      card_body(
        div(
          style = "text-align: center; padding: 20px;",
          uiOutput("audio_player"),
          div(class = "mt-3", textOutput("status", inline = TRUE))
        )
      )
    ),

    # History Card
    card(
      card_header("Generation History", class = "bg-light"),
      card_body(
        DTOutput("history_table"),
        p(
          class = "text-muted small mt-2",
          "Click a row to replay previous generations."
        )
      )
    )
  )
)

server <- function(input, output, session) {
  status_message <- reactiveVal("Ready.")
  audio_path <- reactiveVal(NULL)

  # History storage: list of lists or data frame
  # We use a reactiveValues to store the history data frame
  history_data <- reactiveValues(
    df = data.frame(
      Timestamp = character(),
      Text = character(),
      Voice = character(),
      Path = character(),
      stringsAsFactors = FALSE
    )
  )

  output$status <- renderText(status_message())

  # Initialize voices if API key is present
  observe({
    req(input$api_key)
    isolate({
      # Only auto-refresh if we haven't loaded voices yet and key looks valid
      if (input$voice_name == "Elli" && nzchar(input$api_key)) {
        # We could trigger a refresh here, but let's leave it manual or on-demand to avoid spamming API on load
        # Actually, better to just let user click refresh or wait for them to synthesize
      }
    })
  })

  observeEvent(input$refresh_voices, {
    if (!nzchar(input$api_key)) {
      status_message("Set an API key to load voices.")
      return(NULL)
    }

    status_message("Loading voices...")

    voices <- tryCatch(
      get_voices(api_key = input$api_key),
      error = function(e) {
        status_message(paste("Error:", e$message))
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

    # Create a unique filename
    fname <- paste0(
      "speech-",
      format(Sys.time(), "%Y%m%d-%H%M%S"),
      "-",
      as.integer(runif(1, 1000, 9999)),
      ".mp3"
    )
    output_file <- file.path(audio_dir, fname)

    tryCatch(
      {
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
        )

        if (file.exists(output_file)) {
          audio_path(output_file)
          status_message(paste("Saved audio."))

          # Update History
          new_entry <- data.frame(
            Timestamp = format(Sys.time(), "%H:%M:%S"),
            Text = input$text,
            Voice = input$voice_name,
            Path = output_file,
            stringsAsFactors = FALSE
          )
          history_data$df <- rbind(new_entry, history_data$df)
        }
      },
      error = function(e) {
        status_message(paste("Error:", e$message))
      }
    )
  })

  output$audio_player <- renderUI({
    path <- audio_path()
    if (is.null(path)) {
      return(div(class = "text-muted", "No audio generated yet."))
    }

    # Use HTML5 audio player
    # We add a random query parameter to force reload if the path happens to be same (unlikely with timestamps)
    src_url <- file.path("elevenlabs_audio", basename(path))

    tags$audio(
      controls = NA,
      autoplay = NA,
      style = "width: 100%;",
      tags$source(src = src_url, type = "audio/mp3")
    )
  })

  # Render History Table
  output$history_table <- renderDT({
    req(nrow(history_data$df) > 0)

    # Show only display columns
    display_df <- history_data$df[, c("Timestamp", "Voice", "Text")]

    datatable(
      display_df,
      selection = "single",
      options = list(
        pageLength = 5,
        dom = "tp",
        ordering = FALSE,
        columnDefs = list(list(
          targets = 2,
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 50 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
            "}"
          )
        ))
      ),
      rownames = FALSE
    )
  })

  # Handle Row Click in History
  observeEvent(input$history_table_rows_selected, {
    row_idx <- input$history_table_rows_selected
    if (length(row_idx)) {
      # Data table rows are 1-based indices into the *current view*, but without ordering/filtering they map to df
      # Since we prepend (rbind), the first row is the newest.
      # Verify if DT matches df order.

      selected_path <- history_data$df$Path[row_idx]
      if (file.exists(selected_path)) {
        audio_path(selected_path)
        status_message(paste(
          "Playing from history:",
          history_data$df$Timestamp[row_idx]
        ))
      } else {
        status_message("Error: File not found.")
      }
    }
  })
}

shinyApp(ui, server)
