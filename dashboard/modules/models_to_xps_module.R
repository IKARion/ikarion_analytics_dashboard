modelToXpsUI <- function(id, label="None") {
  
  ns <- NS(id)
  
  fluidRow(
    downloadButton("downloadModel", paste("Download", label)),
    box(
      title = paste("Send", label, "to XPS"),
      selectInput(ns("interval"), "Interval", c("once", "minute", "day")),
      actionButton(ns("model_to_xps"), "Send")
    )
  )
}

modelToXps <- function(input, output, session, model) {
  
  
}