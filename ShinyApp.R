library(shiny)
library(binplot2)
ui <- fluidPage(
  titlePanel("Binplot panel"),
  sidebarLayout(position = "left",
        sidebarPanel("sidebar panel",
                     h2('The uploaded file data'),
                     dataTableOutput('mytable'),
                     fileInput('file', 'Choose info-file to upload',
                               accept = c(
                                 'text/csv',
                                 'text/comma-separated-values',
                                 'text/tab-separated-values',
                                 'text/plain',
                                 '.csv',
                                 '.tsv'
                               )
                     ),
                     # Taken from: http://shiny.rstudio.com/gallery/file-upload.html
                     tags$hr(),
                     checkboxInput('header', 'Header', TRUE),
                     radioButtons('sep', 'Separator',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tab='\t'),
                                  ','),
                     radioButtons('quote', 'Quote',
                                  c(None='',
                                    'Double Quote'='"',
                                    'Single Quote'="'"),
                                  '"'),
                     ################################################################

                     actionButton("choice1", "incorporate external information for column1"),

                     selectInput("column1", "Select Columns", choices = NULL), # no choices before uploading
                     tableOutput("table_display1"),

                     actionButton("choice2", "incorporate external information for column2"),

                     selectInput("column2", "Select Columns", choices = NULL), # no choices before uploading

                     tableOutput("table_display2")),
            mainPanel("main panel",
                  plotOutput("plot_display"))))

server <- function(input, output, session) { # added session for updateSelectInput

  info1 <- eventReactive(input$choice1, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)

    # Changes in read.table
    f1 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f1)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column1","Select Columns", choices = vars)
    f1
  })

  info2 <- eventReactive(input$choice2, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)

    # Changes in read.table
    f2 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f2)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column2","Select Columns", choices = vars)
    f2
  })

  output$table_display1 <- renderTable({
    f <- info1()
    f <- subset(f, select = input$column1) #subsetting takes place here
    head(f)
  })
  output$table_display2 <- renderTable({
    f <- info2()
    f <- subset(f, select = input$column2) #subsetting takes place here
    head(f)
  })

  output$plot_display <- renderPlot({
    f1 <- info1()
    x1 <- subset(f1, select = input$column1)
    f2 <- info2()
    x2 <- subset(f2, select = input$column2)
    binplot(unlist(x1), unlist(x2),nr=500, nc=500, "l")
  })
}

shinyApp(ui, server)
