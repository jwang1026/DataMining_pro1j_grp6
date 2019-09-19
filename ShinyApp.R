ui <- fluidPage(
  plotOutput("Shiny App plot")
)
server <- function(input, output) {
  output$hist<-renderPlot({
    plot(x,y)
  })
}

shinyApp(ui = ui, server = server)
