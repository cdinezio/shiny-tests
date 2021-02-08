library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shinydashboard)
library(forcats)



# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Argentina",
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
        width = 320,
        sidebarMenu(
            id = "menu_principal",
            menuItem(text = "Informacion general", tabName = "info_general",icon = icon("dashboard"),badgeLabel = "new"),
            menuItem(text = "VER QUE MINISTERIO ES", tabName = "minist",icon = icon("dashboard"),
                     menuSubItem(text = "Solicitud portación de armas", tabName = "armas_fuego")),
            menuItem(text = "Min. de Desarrollo Social de la Nación", tabName = "min_des_social",icon = icon("dashboard"),
                     menuSubItem(text = "Progresar trabajo",tabName = "progresar_trabajo")),
            menuItem(text = "Min. del Interior, Obras Públicas y Vivienda", tabName = "min_int_obrp_vi",icon = icon("dashboard")),
            menuItem(text = "MINISTERIO 2", tabName = "c",icon = icon("dashboard")),
            menuItem(text = "MINISTERIO 3", tabName = "d",icon = icon("dashboard"))
            
        ),
        sidebarMenu(
            id = "menu_secundario",
            menuItem(text = "Dasboard1", tabName = "dashboard",icon = icon("dashboard"),badgeLabel = "new"),
            menuItem(text = "Widgets1", tabName = "widgets",icon = icon("dashboard"))
        )
    ),
    
    
    
    # Body content
    # Adentro le pones tabItems, para referenciar los menuItem de arriba.
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom_argentina.css")
        ),
        tabItems(
            tabItem(tabName = "info_general",
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
        ### Armas de fuego    
        tabItem(tabName = "armas_fuego",
                fluidRow(
                box(title = "Solicitudes por genero",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput(outputId = "armas_genero_2019", height = 1080)
                    ),
                box(title = "Solicitudes por mes",
                    solidHeader = TRUE,
                    width = 8,
                    plotOutput(outputId = "armas_meses_2019", height = 1080)
                )
                )
                ),
        ## Progresar
        tabItem(tabName = "progresar_trabajo",
                fluidRow(
                        infoBoxOutput(outputId = "total_gastos_programa"),
                        infoBox(title = "Cantidad de beneficiarios:"),
                        infoBox(title = "Monto mensual:"),
                        ),
                fluidRow(
                    box(title = "Desembolsos - Progresar Trabajo",
                        solidHeader = TRUE,
                        status = "info",
                        collapsible = TRUE,
                        width = 12,
                        plotOutput(outputId = "potenciar_trabajo_total",height = 1000)
                    )
        
        )
    )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    armas_2019 <- read_csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/armas_2019.csv") %>% mutate(fecha_publicacion = dmy(fecha_publicacion))
    potenciar_trabajo_2020 <- read_csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/potenciar_trabajo.csv") %>% filter(!is.na(provincia))
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    ### Armas de fuego - Output
    output$armas_genero_2019 <- renderPlot({
        
        armas_2019 %>% count(genero) %>% filter(genero %in% c("masculino","femenino")) %>% ggplot(aes(genero,n, fill = genero)) + geom_col() + theme(legend.position = 0)
    })
    output$armas_meses_2019 <- renderPlot({
        #armas_2019 <- read_csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/armas_2019.csv")
        armas_2019 %>% mutate(mes = month(fecha_publicacion)) %>% count(mes,genero) %>% 
            filter(genero %in% c("masculino","femenino")) %>% ggplot(aes(as.factor(mes),n, fill = genero)) + geom_col(position = "dodge") +
            scale_y_continuous(breaks = seq(0,6000,1000)) + theme(legend.position = 0)
    })
    
    
     output$potenciar_trabajo_total <- renderPlot({
        
        potenciar_trabajo_2020 %>% count(provincia, wt = n) %>% mutate(provincia = fct_reorder(provincia,n), n = n / 1000000000) %>% ggplot(aes(provincia,n)) + geom_col(fill = "#1d3557") + theme(legend.position = 0) +
            coord_flip()  + scale_y_continuous(breaks = seq(0,32,2)) +
                labs(title = "Pagos por el programa Progresar Trabajo", subtitle = "En miles de millones de pesos") +
                    theme(axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15),plot.title = element_text(size = 25))
        
    })
    
    output$total_gastos_programa <- renderInfoBox({
        infoBox(title = "Total invertido en el programa durante 2020:",value = paste0("$",format(x = potenciar_trabajo_2020 %>% pull(n) %>% sum,big.mark = ",")))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
