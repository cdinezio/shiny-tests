library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(shinydashboard)
library(forcats)

nombres <- read_csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/nombres.csv")
autos_robados <- read_csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/autos_robados.csv") %>% arrange(automotor_marca_descripcion) %>% mutate(titular_domicilio_localidad = str_to_title(titular_domicilio_localidad))
condenados <- read.csv2("https://raw.githubusercontent.com/cdinezio/shiny-tests/main/datasets/condenados.csv",encoding = "UTF-8" ) %>% as_tibble() %>% mutate(delito = as.character(delito)) %>% 
    #mutate(delito = str_replace_all(delito,"ãƒ<U+00B0>|ãƒ<U+FFFD>","ú"),delito = str_replace_all(delito,"ãƒâ³M","óm"),delito = str_replace_all(delito,"<U+FFFD>","N")) %>%
    mutate(delito = case_when(
        str_detect(delito,"Estupef|23737") ~ "Tenencia y Trafico de Estupefacientes",
        str_detect(delito,"Contra La Administraci") ~ "Contra la Administracion Publica",
        str_detect(delito,"Financiero") ~ "Contra el ordern Economico y Financiero",
        str_detect(delito,"Contra La Seguridad P") ~ "Contra la Seguridad Publica",
        TRUE ~ delito
    )) %>% filter(delito %in% c("Contra La Propiedad","Tenencia y Trafico de Estupefacientes","Contra Las Personas","Contra La Integridad Sexual","Contra La Libertad","Contra la Administracion Publica","Otras Leyes","Contra la Seguridad Publica","Contra el ordern Economico y Financiero"))

# Defino la UI
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Argentina, en un vistazo",titleWidth = 350),
    
    
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
                     menuSubItem(text = "Denuncias de robo de vehiculo", tabName = "autos_robados"),
                     menuSubItem(text = "Condenados en juicio", tabName = "condenados_tab")),
            menuItem(text = "Min. de Desarrollo Social de la Nación", tabName = "min_des_social",icon = icon("dashboard"),
                     menuSubItem(text = "Progresar trabajo",tabName = "progresar_trabajo")),
            menuItem(text = "Min. del Interior, Obras Públicas y Vivienda", tabName = "min_int_obrp_vi",icon = icon("dashboard"),
                     menuSubItem(text = "Asignación de nombres",tabName = "nombres"))
            )
        ),
    
    
    # Body content
    dashboardBody(
        #tags$head(
         #   tags$link(rel = "stylesheet", type = "text/css", href = "custom_argentina.css")
        #),
        tabItems(
        ########## Informacion general
            tabItem(tabName = "info_general",
                    column(
                        width = 6,
                        fluidRow(infoBox(
                            title = "FUENTE",
                            value = "Los datos provienen de www.datos.gob.ar, y son preprocesados para facilitar su manejo. Los datos usados pueden encontrarse en Github.",
                            width = 12)),
                        fluidRow(infoBox(
                            title = "INFORMACION",
                            value = "Para ver los diferentes graficos disponibles, haga click en las pestañas de la izquierda para desplegar el menu. De allí, elija la categoría de su interés",
                            width = 12)),
                        fluidRow(infoBox(
                            title = "INTERES",
                            value = "Los gráficos son meramente informativos. No hay intención de resaltar ninguna característica en particular. De parecer, es mera coincidencia.",
                            width = 12)),
                        fluidRow(infoBox(
                            title = "TEMPORALIDAD",
                            value = "Todos los graficos contienen datos de 2019. La actualizacion para incluir datos del 2020 está en curso",
                            width = 12))
                    ),
                    column(
                    width = 6,
                    box(title = "Sobre mi", solidHeader = TRUE,status = "warning", height = 500,width = 12),
                    ),
                
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
        ####################################################################################################################
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
        ####################################################################################################################
        ### Condenados en juicio
        tabItem(
            tabName = "condenados_tab",
            fluidRow(
                    box(
                        status = "info",
                        solidHeader = TRUE,
                        title ="Seleccione una opcion para modificar los datos (uno a la vez):" ,
                        checkboxInput("condenado_sexo","Graficar por sexo"),
                        checkboxInput("condenado_profesion","Graficar por profesion"),
                        ),
                ),
            fluidRow(plotOutput("condenados_plot", height = 1000))
            
        ),
        ####################################################################################################################
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
    
    ### Condenados en juicio - Plot
    output$condenados_plot <- renderPlot({
        if (input$condenado_sexo) {
            condenados %>% filter(genero %in% c("Masculino","Femenino","Trans")) %>% count(genero,delito) %>% mutate(delito = fct_reorder(delito,n,sum)) %>% ggplot(aes(delito,n, fill = genero)) + geom_col(position = "dodge") +
                theme(legend.position = "bottom",axis.text.x = element_text(size = 15))  + xlab("") + ylab("Cantidad de condenados")
        } else if (input$condenado_profesion) {
            top_profesiones <- condenados %>% count(profesion, sort = TRUE) %>% slice(1:10) %>% pull(profesion)
            condenados %>% filter(genero %in% c("Masculino","Femenino","Trans"), profesion %in% top_profesiones) %>% count(genero,profesion,delito) %>% mutate(delito = fct_reorder(delito,n,sum)) %>% ggplot(aes(delito,n, fill = profesion)) + geom_col(position = "dodge") +
                theme(legend.position = "bottom",axis.text.x = element_text(size = 15))  + xlab("") + ylab("Cantidad de condenados")
        } else {
            condenados %>% filter(genero %in% c("Masculino","Femenino","Trans")) %>% count(delito) %>% mutate(delito = fct_reorder(delito,n,sum)) %>% ggplot(aes(delito,n, fill = delito)) + geom_col() +
                theme(legend.position = 0,axis.text.x = element_text(size = 15))  + xlab("") + ylab("Cantidad de condenados")
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
