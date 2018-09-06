userActiveDaysUI <- function(id, label="Active Days") {
  
  ns <- NS(id)
  fluidPage(
    fluidRow(
      valueBoxOutput("activeDaysBox"),
      box(
        title = "Active students (days)",
        plotOutput(ns("activeDaysPlot")) %>% withSpinner(color="grey")
      )
    ),
    fluidRow(
      box(
        title = "Active days (cumulative)",
        plotOutput(ns("activeDaysCumPlot")) %>% withSpinner(color="grey"),
        sliderInput(ns("active_days_reference"), "Reference point", min = 1, max=30, value=2)
      ),
      box(
        title="Users below reference",
        DT::dataTableOutput(ns("activeDaysUsers"))
      )
    )
  )
}

userActiveDays <- function(input, output, session, courses, timeRange) {
  
  activeDaysModel <- reactive({
    getActiveDaysAll(courses()) %>%
      mutate(activeDay=ymd(activeDay))
  })
  
  activeDays <- reactive({
    trange <- timeRange() 
    
    activeDaysModel() %>% filter((activeDay >= trange[1]) & (activeDay <= trange[2]))
  })
  
  output$activeDaysBox <- renderValueBox({
    
    valueBox(
      activeDays()$user %>% unique %>% length,
      subtitle = "Number of active students",
      icon = icon("user"),
      color = "purple"
    )
  })
  output$activeDaysPlot <- renderPlot({
    
    activeDays() %>%
      count(activeDay) %>%
      ggplot(aes(x=activeDay, y=n)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90))
  })
  
  output$activeDaysCumPlot <- renderPlot({
    
    activeDays() %>%
      count(user) %>%
      ggplot(aes(x=n)) + stat_ecdf() + 
      geom_vline(xintercept = input$active_days_reference) +
      xlab("active days") +
      ylab("% users") +
      theme_bw()
  })
  
  output$activeDaysUsers <- DT::renderDataTable(
    activeDays() %>%
      count(user, sort=TRUE) %>%
      filter(n < input$active_days_reference) %>%
      rename("Active Days" = n) %>%
      DT::datatable()
  )
  
  activeDays
}