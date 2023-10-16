
library(tidyr)
library(readr)
library(dplyr)
library(shinythemes)
library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(readxl)
library(RCurl)
library(sf)
library(tidyverse)
library(stringr)
library(extrafont)
library(showtext)
library(fresh)
library(shinyWidgets)
library(DT)
library(leafpop)
font_add_google("Montserrat", "montserrat")
library("thematic")
rm(list = ls())
showtext_auto()
muns <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios_con_menos_islas_aun.geojson")
split1 <- t(sapply(muns$CVEGEO, function(x) substring(x, first=c(1,3), last=c(2,5))))
muns<-cbind(muns, split1)
muns$CVE_MUN<-paste0(muns$X1,"-", muns$X2)
muns$ENTIDAD <- muns$NOM_ENT
muns$MUNICIPIO <- muns$NOM_MUN
#tm2 <- st_read("~/Documents/dashMP24_cm/vias_de_tren_maya_completo.shp")
#tm2 <- st_read("https://github.com/claudiodanielpc/macroproyectos/raw/main/inputs/vias_de_tren_maya_completo.shp")
#tm <- st_transform(tm2, 4326)
tm2 <- st_read("https://raw.githubusercontent.com/claudiodanielpc/macroproyectos/main/inputs/tren_maya.geojson")
tm <- st_transform(tm2, 4326)
data <- read.csv("https://raw.githubusercontent.com/claudiodanielpc/macroproyectos/main/outputs/consolidado.csv",fileEncoding = "Latin1", check.names = F)


data <- data %>% select("clave","proyecto","sector","subsector","desc_unidad","fecha_inicio", "fecha_fin",
                        "desc_proyecto","estados","cantidad","medida", "costo_total")

names(data) <- c("Clave","Proyecto","Sector","Subsector","Unidad responsable","Fecha inicio", "Fecha fin",
                 "Descripción proyecto","Estados","Cantidad","Medida", "Costo total")

prueba <- data
reds <- colorNumeric("Spectral", domain =NULL, reverse = T)
# Define UI ----


ui <- fluidPage(
  title="Macroproyectos 2024 México",
  
  
  #titlePanel(title = span(img(src = "https://www.cenmex.com/image/catalog/logo-head-color-03.png", height = 50,
  #                            width = 200, align = "center"), h2(strong("Macroproyectos 2024 México"), align = "center"))),
  #tags$h2("Add a shiny app background image"),
  #setBackgroundImage(
  #  src = "https://www.cenmex.com/image/catalog/logo-head-color-03.png"
  #),
  
  tags$style('.container-fluid {
                             background-color: #f0ece0;
             }'),
  #
  use_googlefont("montserrat"),
  use_theme(create_theme(
    theme = "default",
    bs_vars_font(
      family_sans_serif = "'montserrat'"
    )
  )),
  
  titlePanel(
    h2(strong("Macroproyectos 2024 México"), align = "center")
  ),
  theme = bslib::bs_theme(bootswatch = "united"),
  sidebarLayout(
    sidebarPanel(
      
      img(
        src = "https://www.cenmex.com/image/catalog/logo-head-color-03.png",
        height = 43,
        width = 175,
        
      ),
      
      
      selectInput("var", h3("Proyecto"), 
                  choices = "Tren Maya", selected = "Tren Maya"),
      
      width=2
    ),
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Mapa", leafletOutput("map", height=1200,width = "100%")),
                  #tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Tabla", DT::dataTableOutput("table") )
      )

    )
  
  )
)


# Define server logic ----
server <- function(input, output){
  
  
  d <- reactive({
    dist <- data
  })
  
  output$table <- renderTable({
    d()
  })
  
  output$table <- renderDataTable({
    
    d()
  })
  
  
  output$map <- renderLeaflet({
    reds <- colorNumeric("Spectral", domain = NULL, reverse = T, na.color = NA)
    
    leaflet(data) %>%
      addPolylines(data=tm, color="red",opacity = 1) %>% 
      setView(lng =  -89.915256,lat =  16.939306,    zoom = 7) %>%  
      addProviderTiles("CartoDB.Positron",layerId = "basetile",options = providerTileOptions(minZoom = 6))
    
    
    
  }) 
  
  observe({
    leafletProxy("map", data=muns)%>% 
      #clearShapes()%>% 
      addPolygons(data=muns,
                  
                  #color = "#000000" ,
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 0.5,
                                                      fillColor = 'blue',
                                                      bringToFront = TRUE),
                  stroke = FALSE, 
                  color = "white",
                  
                  popup = ~leafpop::popupTable(muns,
                                               zcol = c("MUNICIPIO", "ENTIDAD"),
                                               row.numbers = FALSE, feature.id = FALSE)
                  
      )
    
    
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

