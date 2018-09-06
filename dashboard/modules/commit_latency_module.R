commitLatencyUI <- function(id, label="Commit Latencies") {
  
  ns <- NS(id)
  
  fluidRow(
    # latency commit message
    box(
      title = "Group Commit Latency",
      plotOutput(ns("CommitLatencyPlot")) %>% withSpinner(color="grey"),
      sliderInput(ns("commit_latency_reference"), "Reference point", min = 1, max=250, value=10)
    ),
    box(
      title="Groups above commit latency (reference)",
      DT::dataTableOutput(ns("CommitLatencyGroups"))
    )
  )
}

commitLatency <- function(input, output, session, courses, timeRange) {
  
  groupGitSequences <- reactive({
    
    df <- getGitSequencesAll()
    getGitSequencesAll()
  })
  
  # get latencies for commits
  groupCommitLatencies <- reactive({
    
    start <- "2018-05-01"
    end <- "2018-05-30"
    all_repos <- getGroupRepositories()
    getCommitLatencies(all_repos[1,]$repo, start, end)
  })
  
  output$CommitLatencyPlot <- renderPlot({
    groupCommitLatencies() %>%
      ggplot(aes(x=latency)) + stat_ecdf() + 
      geom_vline(xintercept = input$commit_latency_reference) +
      xlab("latency") +
      ylab("% groups") +
      theme_bw()
  })
  
  output$CommitLatencyGroups <- DT::renderDataTable(
    groupCommitLatencies() %>%
      #filter(latency > input$commit_latency_reference) %>%
      DT::datatable()
  )
  
  output$GroupRepos <- DT::renderDataTable(
    
    getGroupRepositories()
  )
}