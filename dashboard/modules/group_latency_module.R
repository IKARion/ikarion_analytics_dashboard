groupLatencyUI <- function(id, label="Group Latencies") {
  
  ns <- NS(id)
  
  fluidRow(
    
    # latency forum post
    box(
      title = "Group Latency",
      plotOutput(ns("latencyPlot")) %>% withSpinner(color="grey"),
      sliderInput(ns("latency_reference"), "Reference point", min = 1, max=250, value=10)
    )
    ,
    box(
      title="Groups above answer latency (reference)",
      DT::dataTableOutput(ns("latencyGroups"))
    )
  )
}

groupLatency <- function(input, output, session, courses, timeRange) {
  
  groupSequences <- reactive({
    
    df <- getGroupSequencesAll(courses())
    getGroupSequencesAll(courses())
  })
  
  groupLatencies <- reactive({
    trange <- timeRange() 
    start <- trange[1]# %>% as.POSIXlt %>% as.integer
    end <- trange[2] #%>% as.POSIXlt %>% as.integer
    
    getGroupLatencies2(groupSequences(), start, end)
  })
  
  output$latencyPlot <- renderPlot({
    groupLatencies() %>%
      ggplot(aes(x=latency)) + stat_ecdf() + 
      geom_vline(xintercept = input$latency_reference) +
      xlab("latency") +
      ylab("% groups") +
      theme_bw()
  })
  
  output$latencyGroups <- DT::renderDataTable(
    groupLatencies() %>%
      filter(latency > input$latency_reference) %>%
      DT::datatable()
  )
  
  list(sequences=groupSequences, latencies=groupLatencies)
}