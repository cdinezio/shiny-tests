library(shiny)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "My dashboard",
                    dropdownMenu(type = "messages", 
                                 messageItem(from = "Cristian",message = "Hola!"),
                                 messageItem(from = "Cristian",message = "Hola!"),
                                 messageItem(from = "Cristian",message = "Hola!"),
                                 messageItem(from = "Cristian",message = "Hola!",)
                                 ),
                    dropdownMenu(type = "notifications", 
                                 notificationItem(text = "AVISO IMPORTANTE",status = "warning"),
                                 notificationItem(text = "AVISO IMPORTANTE", status = "danger"),
                                 notificationItem(text = "AVISO IMPORTANTE")
                    ),
                    dropdownMenu(type = "tasks", 
                                 taskItem(value = 90, color = "green",text = "Docu check"),
                                 taskItem(value = 40, color = "red",text = "Docu check"),
                                 taskItem(value = 90, color = "yellow",text = "Docu check")
                    )
                    
                    
                    
                    ),
    
    # Sidebar content
    # Creas un sidebar, y adentro le pones el sidebarMenu() (importante el id), y adentro de eso los items
    # que queres ver en el sidebar. 
    dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            menuItem(text = "Dasboard", tabName = "dashboard",icon = icon("dashboard"),badgeLabel = "new"),
            menuItem(text = "Widgets", tabName = "widgets",icon = icon("dashboard"))
        ),
        sidebarMenu(
            id = "tabss",
            menuItem(text = "Dasboard1", tabName = "dashboard",icon = icon("dashboard"),badgeLabel = "new"),
            menuItem(text = "Widgets1", tabName = "widgets",icon = icon("dashboard"))
        )
    ),
    
    
    
    # Body content
    # Adentro le pones tabItems, para referenciar los menuItem de arriba.
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow(
                    box(title = "GRAFICO", solidHeader = TRUE,status = "warning",plotOutput("plot1", height = 500)),
                    infoBox(title = "asd"),
                    tabBox(
                        tabPanel("hola",icon = shiny::icon("gear")),
                        tabPanel("otra"),
                        
                        height = 200),
                    box(
                        title = "Controls",
                        sliderInput("slider", "Number of observations:", min = 0,max = 25,value = 10),
                        textInput("id1","Insert your name:"),
                        collapsible = TRUE,
                        footer = "Select the relevant properties",
                        status = "info",
                        p("I dont know where this is ending up"),
                            )
                        )
            ),
        tabItem(tabName = "widgets",
                h2("Widgets tab"))
        )
    ),


)

# Define server logic required to draw a histogram
server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
