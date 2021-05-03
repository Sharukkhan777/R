library(lubridate)
library(shiny)

ui <- fluidPage(
  hr(),
  actionButton(inputId = 'start',label = 'Start'),
  actionButton(inputId = "increment",label = "Increment"),
  actionButton(inputId = "decrement",label = "Decrement"),
  h1("Speed"),
  textOutput(outputId = "speed"),
  h1("Time"),
  textOutput(outputId = "time"),
  h1("Distance"),
  textOutput("distance")
)

server <- function(input, output, session){
  output$speed <- renderText({speed()})
  output$time <- renderText("1/60")
  # output$distance <- renderText({"distance default"})
  
  speed <- reactiveVal(20)
  distance <- reactiveVal(0)
  active <- reactiveVal(FALSE)
  
  output$distance <- renderText({distance()})
  
  observe({
    invalidateLater(100, session)
    isolate({
      if(active())
      {
        distance(distance()+(speed()*0.01))
        if(distance()>400)
        {
          active(FALSE)
          showModal(modalDialog(
            title = "Important message",
            "Countdown  completed!"
          ))
        }
      }
    })
  })

  # observers for actionButtons
  observeEvent(input$start, {active(TRUE)})
  observeEvent(input$increment, {speed(speed()+5)})
  observeEvent(input$decrement, {speed(speed()-5)})
}

shinyApp(ui, server)