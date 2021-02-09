library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shinydashboard)
library(forcats)

nombres <- read_csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/nombres.csv")
autos_robados <- read_csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/autos_robados.csv") %>% arrange(automotor_marca_descripcion) %>% mutate(titular_domicilio_localidad = str_to_title(titular_domicilio_localidad))

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
            menuItem(text = "Min. de Justicia y Derechos Humanos", tabName = "min_just_der_hum",icon = icon("dashboard"),
                     menuSubItem(text = "Solicitud portación de armas", tabName = "armas_fuego"),
                     menuSubItem(text = "Denuncias de robo de vehiculo", tabName = "autos_robados")),
            menuItem(text = "Min. de Desarrollo Social de la Nación", tabName = "min_des_social",icon = icon("dashboard"),
                     menuSubItem(text = "Progresar trabajo",tabName = "progresar_trabajo")),
            menuItem(text = "Min. del Interior, Obras Públicas y Vivienda", tabName = "min_int_obrp_vi",icon = icon("dashboard"),
                     menuSubItem(text = "Asignación de nombres",tabName = "nombres")),
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
        ########## Informacion general
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
        ##########################################################################################
        ### Armas de fuego    
        tabItem(tabName = "armas_fuego",
                fluidRow(
                box(title = "Solicitudes totales por genero",
                    status = "info",
                    solidHeader = TRUE,
                    width = 4,
                    plotOutput(outputId = "armas_genero_2019", height = 1080)
                    ),
                box(title = "Solicitudes por mes",
                    status = "info",
                    solidHeader = TRUE,
                    width = 8,
                    plotOutput(outputId = "armas_meses_2019", height = 1080)
                )
                )
                ),
        ### Autos robados
        tabItem(
            tabName = "autos_robados",
            fluidRow(
                column(
                    width = 4,
                    box(title ="Seleccione una marca:" ,selectInput("auto_marca",label = "Despliegue la lista para ver las opciones", choices = unique(autos_robados$automotor_marca_descripcion)))),
                column(
                    width = 5,
                    box(title = "Checkboxes",
                    checkboxInput("auto_zona","Ver zona de residencia?"),
                    checkboxInput("sacar_caba","Remover CABA de residencia?"),
                    )
                    )

                
                ),
            fluidRow(plotOutput("autos_robados_plot", height = 1080))
                   
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
            ),
        ### Nombres
        tabItem(tabName = "nombres",
                fluidRow(
                    box(
                    width = 4,
                    title = "Seleccioná un nombre de la lista:",
                    solidHeader = TRUE,
                    status = "info",
                    selectInput(inputId = "nombres_input",label = "Seleccioná un nombre:",choices = unique(nombres$nombre))),
                ),
                fluidRow(
                    box(title = "Evolución del nombre elegido:",
                        solidHeader = TRUE,
                        status = "info",
                        collapsible = TRUE,
                        width = 12,
                        plotOutput(outputId = "nombres_plot",height = 1000)
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
        
        armas_2019 %>% count(genero) %>% filter(genero %in% c("masculino","femenino")) %>% 
            ggplot(aes(genero,n, fill = genero)) + geom_col() + theme(legend.position = 0,axis.text.x = element_text(size = 15),axis.text.y = element_text(face = "bold", size = 15)) +
            scale_y_continuous(labels = scales::comma) + ylab("")
            
            })
    output$armas_meses_2019 <- renderPlot({
        #armas_2019 <- read_csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/armas_2019.csv")
        armas_2019 %>% mutate(mes = month(fecha_publicacion)) %>% count(mes,genero) %>% 
            filter(genero %in% c("masculino","femenino")) %>% ggplot(aes(as.factor(mes),n, fill = genero)) + geom_col(position = "dodge") +
            scale_y_continuous(breaks = seq(0,6000,500), labels = scales::comma) + theme(legend.position = 0,axis.text.x = element_text(size = 15),axis.text.y = element_text(face = "bold", size = 15)) + xlab("") 
    })
    
    ### Autos robados - output
    output$autos_robados_plot <- renderPlot({
        if (input$auto_zona) {
            if (input$sacar_caba) {
                autos_robados %>% filter(automotor_marca_descripcion == input$auto_marca, titular_domicilio_localidad != "C.autonoma De Bs.as") %>% count(titular_domicilio_localidad, sort = TRUE) %>% 
                    mutate(titular_domicilio_localidad = fct_reorder(titular_domicilio_localidad,n,sum)) %>%
                    slice(1:20) %>% ggplot(aes(titular_domicilio_localidad,n, fill = titular_domicilio_localidad)) + geom_col() + 
                    theme(axis.text.x = element_text(angle = 90,size = 15,vjust = 0.5),axis.text.y = element_text(face = "bold", size = 15), legend.position = 0) +
                    coord_flip() + xlab("") + ylab("Cantidad de robos denunciados durante 2019") +
                    scale_fill_viridis_d(option = "D", direction = -1)
            } else {
            autos_robados %>% filter(automotor_marca_descripcion == input$auto_marca) %>% count(titular_domicilio_localidad, sort = TRUE) %>% 
                mutate(titular_domicilio_localidad = fct_reorder(titular_domicilio_localidad,n,sum)) %>%
                slice(1:20) %>% ggplot(aes(titular_domicilio_localidad,n, fill = titular_domicilio_localidad)) + geom_col() + 
                theme(axis.text.x = element_text(angle = 90,size = 15,vjust = 0.5),axis.text.y = element_text(face = "bold", size = 15), legend.position = 0) +
                coord_flip() + xlab("") + ylab("Cantidad de robos denunciados durante 2019") +
                scale_fill_viridis_d(option = "D", direction = -1) }
        } else {
            autos_robados %>% filter(automotor_marca_descripcion == input$auto_marca) %>% count(automotor_modelo_descripcion, sort = TRUE) %>% 
                mutate(automotor_modelo_descripcion = fct_reorder(automotor_modelo_descripcion,n,sum)) %>%
                slice(1:20) %>% ggplot(aes(automotor_modelo_descripcion,n, fill = automotor_modelo_descripcion)) + geom_col() + 
                theme(axis.text.x = element_text(angle = 90,size = 15,vjust = 0.5),axis.text.y = element_text(face = "bold", size = 15), legend.position = 0) +
                coord_flip() + xlab("") + ylab("Cantidad de robos denunciados durante 2019") +
                scale_fill_viridis_d(option = "D", direction = -1) 
        }
    })
    
    
    ### Potenciar trabajo
     output$potenciar_trabajo_total <- renderPlot({
        
        potenciar_trabajo_2020 %>% count(provincia, wt = n) %>% mutate(provincia = fct_reorder(provincia,n), n = n / 1000000000) %>% ggplot(aes(provincia,n)) + geom_col(fill = "#1d3557") + theme(legend.position = 0) +
            coord_flip()  + scale_y_continuous(breaks = seq(0,32,2)) +
                labs(title = "Pagos por el programa Progresar Trabajo", subtitle = "En miles de millones de pesos") +
                    theme(axis.text.x = element_text(size = 15),axis.text.y = element_text(size = 15),plot.title = element_text(size = 25)) + ylab("")
        
    })
    
    output$total_gastos_programa <- renderInfoBox({
        infoBox(title = "Total invertido en el programa durante 2020:",value = paste0("$",format(x = potenciar_trabajo_2020 %>% pull(n) %>% sum,big.mark = ",")))
    })
    
    
    
    ### Nombres
    output$nombres_plot <- renderPlot({
        nombres %>% filter(nombre == input$nombres_input) %>% ggplot(aes(anio,cantidad, fill = anio)) + geom_col() +
            scale_x_continuous(breaks = seq(1920,2020,5)) + xlab("Año") + ylab("Cantidad de personas registradas con el nombre") + scale_fill_viridis_c(option = "D",direction = -1) +
                theme(legend.position = 0)
            
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
