library(binplot2)
library(shiny)

#simulate data;
#x = rnorm(10000) ; y = rnorm(10000)
#ux = rnorm(5000)/3
#uy = ux^2 -0.5
#D<-data.frame(y,uy,x,ux)
#write.table(D,file="temp.txt",sep=",",row.names = FALSE,col.names = FALSE,)



ui <- fluidPage(
  titlePanel("Binplot panel"),
  sidebarLayout(position = "left",
                sidebarPanel("Setting Parameters",
                             h2('Please upload data file.'),
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
                            
                             radioButtons('colorScale', 'Please choose the function for the Color Scale in the binplot.',
                                          c(
                                            'z=log(1+z)'='l',
                                            'z=2*z'='r',
                                            'z=z^2'='t'),'raw'),
                             ################################################################
                             
                             sliderInput(inputId="nrnc",
                                         label="Pleease choose a number for variables nr and nc in the binplot.",
                                         value=1,min=1,max=1000),
                            
                     
                             
                             actionButton("choice1", "Select column1 for binplot!"),
                             selectInput("column1", "Select Columns", choices = NULL), # no choices before uploading
                             tableOutput("table_display1"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
                             actionButton("choice2", "Select column2 for binplot!"),
                             selectInput("column2", "Select Columns", choices = NULL), # no choices before uploading
                             tableOutput("table_display2"),
                             
                             actionButton("choice3", "Select column3 for binplot!"),
                             selectInput("column3", "Select Columns", choices = NULL), # no choices before uploading
                             tableOutput("table_display3"),
                             
                             
                             actionButton("choice4", "Select column4 for binplot!"),
                             selectInput("column4", "Select Columns", choices = NULL), # no choices before uploading
                             tableOutput("table_display4"),
                
                             actionButton("choice5", "Select column1 for histogram!"),
                            selectInput("column5", "Select Columns", choices = NULL), # no choices before uploading
                
                            actionButton("choice6", "Select column2 for histogram!"),
                            selectInput("column6", "Select Columns", choices = NULL)), # no choices before uploading
               
                mainPanel("Plots",
                          plotOutput("plot_display"),plotOutput("plot_display2"),
                          plotOutput("plot_histogram"))))

server <- function(input, output, session) { # added session for updateSelectInput
  
  info1 <- eventReactive(input$choice1, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    
    # Changes in read.table
    f1 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f1)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column1","Column 1 for binplot:", choices = vars)
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
    updateSelectInput(session, "column2","Column 2 for binplot:", choices = vars)
    f2
  })
  
  info3 <- eventReactive(input$choice3, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    
    # Changes in read.table
    f3 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f3)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column3","Column 3 for binplot:", choices = vars)
    f3
  })
  
  info4 <- eventReactive(input$choice4, {
    inFile <- input$file
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    
    # Changes in read.table
    f4 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f4)
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "column4","Column 4 for binplot:", choices = vars)
    f4
  })
  
  info5 <- eventReactive(input$choice5, {
    inFile <- input$file
    req(inFile)
    f5 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f5)
    updateSelectInput(session, "column5","Select Column 1 for histogram.", choices = vars)
    f5
  })
  
  info6 <- eventReactive(input$choice6, {
    inFile <- input$file
    req(inFile)
    f6 <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f6)
    updateSelectInput(session, "column6","Select Column 2 for histogram.", choices = vars)
    f6
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
  output$table_display3 <- renderTable({
    f <- info3()
    f <- subset(f, select = input$column3) #subsetting takes place here
    head(f)
  })
  output$table_display4 <- renderTable({
    f <- info4()
    f <- subset(f, select = input$column4) #subsetting takes place here
    head(f)
  })
  
  
  output$plot_display <- renderPlot({
    f1 <- info1()
    y <- subset(f1, select = input$column1)
    f2 <- info2()
    uy <- subset(f2, select = input$column2)
    f3 <- info3()
    x <- subset(f3, select = input$column3)
    f4 <- info4()
    ux <- subset(f4, select = input$column4)
    
    binplot(c(unlist(y),unlist(uy))+20,c(unlist(x),unlist(ux)),nr=input$nrnc,nc=input$nrnc)
    
    
  })
  
  output$plot_display2 <- renderPlot({
    f1 <- info1()
    y <- subset(f1, select = input$column1)
    f3 <- info3()
    x <- subset(f3, select = input$column3)
    
    binplot(unlist(y), unlist(x),nr=input$nrnc, nc=input$nrnc, scale=input$colorScale)
    
  })
  
  output$plot_histogram <- renderPlot({
    f5 <- info5()
    x <- subset(f5, select=input$column5)
    f6 <- info6()
    y <- subset(f6, select = input$column6)
    scatterhist(unlist(x),unlist(y))
  })
}

shinyApp(ui, server)
