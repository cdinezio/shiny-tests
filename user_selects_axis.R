library(shiny)
library(ggplot2)
library(thematic)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file", NULL, accept = c(".csv", ".tsv")),
      numericInput("n", "Rows", value = 5, min = 1, step = 1),
      textInput("ejex", "Write X axis name",value = "A"),
      textInput("ejey", "Write Y axis name", value = "B"),
      checkboxInput("color","Colour by some variable?"),
      textInput("color_var", "If yes, which one?",value = "G"),
      actionButton("graficar","Graficar los datos"),
      sliderInput("sizePoints","Size of points:",min = 1,max = 20,value = 3),
      textInput("title","Plot title:"),
      tableOutput("head"),
      verbatimTextOutput("datos")),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    req(input$graficar)
    
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ";"),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
    
    
  })
  
  output$head <- renderTable({
  })
  
  output$datos <- renderPrint({input$ejex})
  
  output$grafico <- renderPlot({
    req(input$graficar)
    data() %>% ggplot(aes_string(input$ejex,input$ejey, colour = if_else(input$color,input$color_var,NULL))) + 
      geom_point(size = input$sizePoints) + labs(title = input$title)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
