modelsToXpsUI <- function(id, label) {
  
  ns <- NS(id)
  
  DT::dataTableOutput(ns("models_to_xps"))
}

modelsToXps <- function(input, output, session) {
  
  output$models_to_xps <- DT::renderDataTable(
    getScheduledTasks() %>%
      DT::datatable()
  )
}