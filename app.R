library(shiny)
library(leaflet)
library(rgdal)
library(raster)
library(rworldmap)

provider = "Esri.WorldImagery"

# africa = readOGR(".", "africa", stringsAsFactors = FALSE, encoding = "")
data(countriesLow)
africa = countriesLow[
  !is.na(countriesLow$continent) & 
    countriesLow$TYPE == "Sovereign country" &
    countriesLow$continent == "Africa"
  , ]
africa$NAME = africa$NAME %>% as.character

rad = raster("CERES_NETFLUX_M_2015-07-01_rgb_1440x720.TIFF")
rad = crop(rad, africa)

ui <- pageWithSidebar(
  headerPanel('Solar Energy App - Demo'),
  sidebarPanel(
    selectInput(
      "country", 
      "Country", 
      choices = c("", africa$NAME), 
      selected = "",
      multiple = FALSE
      ),
    checkboxInput("rad", label = "Radiation Layer", value = FALSE),
    actionButton("refresh", "Start Over!"),
    tableOutput('txt')
  ),
  mainPanel(
    leafletOutput("mymap")
  )
  )

server = function(input, output, session) {

  filtered = reactive({
    if(input$country == "") africa else africa[africa$NAME == input$country, ]
  })
  
  output$mymap = renderLeaflet({
    leaflet() %>%
      # fitBounds(lng1=-16, lat1=-30, lng2=51, lat2=37) %>% 
      addProviderTiles(provider) %>% 
      addPolygons(
        data = filtered(), 
        layerId = filtered()$COUNTRY,
        group = "country",
        color = "yellow",
        fill = FALSE,
        dashArray = "5, 10",
        stroke = input$country != ""
        )
  })
  
  pal = colorNumeric(c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"), values(rad),
    na.color = "transparent")
  
  observe({
    leafletProxy("mymap", session) %>% 
      addProviderTiles(provider) %>% 
      clearImages()
  })
  
  observe({
    if(input$rad) {
      leafletProxy("mymap", session) %>% 
        addProviderTiles(provider) %>% 
        addRasterImage(rad, colors = pal, opacity = 0.5) %>% 
        addLegend(pal = pal, values = values(rad), title = "Watt / m^2")
    } else {
      leafletProxy("mymap", session) %>% 
        addProviderTiles(provider) %>% 
        clearImages() %>% 
        clearControls}
  })
  
  
  observe({
    leafletProxy("mymap", session) %>% 
      clearGroup(group = "pol") %>% 
      addPolygons(
        data = as.matrix(values$df),
        group = "pol"
      )
  })
  
  last_point = reactive({
      data.frame(
        lon = input$mymap_click[[2]] %>% as.numeric, 
        lat = input$mymap_click[[1]] %>% as.numeric
        )
  })
  
#   pol = reactive({
#     pol = 
#       values$df %>% 
#       as.matrix %>% 
#       Polygon %>% 
#       list %>% 
#       Polygons(ID = "a") %>% 
#       list %>% 
#       SpatialPolygons(proj4string = CRS("+proj=longlat +datum=WGS84"))
#     })
  
  values = reactiveValues(
    df = data.frame(lon = numeric(0), lat = numeric(0))
    )
  observe({
      values$df = rbind(isolate(values$df), last_point())
  })      

  observeEvent(input$refresh, {
    values$df = data.frame(lon = numeric(0), lat = numeric(0))
  }) 
  observeEvent(input$country, {
    values$df = data.frame(lon = numeric(0), lat = numeric(0))
  }) 
  
  output$txt = renderTable(values$df)
  
#   output$area = 
#     gArea()
  
}

shinyApp(ui, server)